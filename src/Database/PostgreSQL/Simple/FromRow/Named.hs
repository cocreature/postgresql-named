{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Database.PostgreSQL.Simple.FromRow.Named
  ( fieldByName
  , deserialize
  , NoSuchColumn(..)
  ) where

import           Control.Exception
import           Control.Monad.Extra
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BS
import           Data.Typeable
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Internal
import           GHC.TypeLits
import           Generics.SOP
import qualified Generics.SOP.Type.Metadata as T

-- | Deserialize a type with a single record constructor by matching
-- the names of columns and record fields. Currently the complexity is /O(n^2)/ where n is the
-- number of record fields.
--
-- This is intended to be used as the implementation of 'fromRow'.
--
-- Throws 'NoSuchColumn' if there is a field for which there is no
-- column with the same name.
deserialize :: forall a modName tyName constrName fields xs.
  ( Generic a
  , HasDatatypeInfo a
  , All2 FromField (Code a)
  , KnownSymbol modName
  , KnownSymbol tyName
  , DatatypeInfoOf a ~ 'T.ADT modName tyName '[ 'T.Record constrName fields]
  , Code a ~ '[xs]
  , T.DemoteFieldInfos fields xs
  ) => RowParser a
deserialize = do
  let f :: forall f. FromField f => FieldInfo f -> RowParser f
      f (FieldInfo name) = fieldByName fromField (BS.fromString name)
  res <-
    fmap (to . SOP . Z) $
    hsequence
      (hcliftA
         (Proxy :: Proxy FromField)
         f
         (T.demoteFieldInfos (Proxy :: Proxy fields)))
  setToLastCol
  pure res

liftIO' :: IO a -> ReaderT Row (StateT PQ.Column Conversion) a
liftIO' = lift . lift . liftConversion

-- | Thrown when there is no column of the given name.
data NoSuchColumn =
  NoSuchColumn ByteString
  deriving (Show, Eq, Ord, Typeable)

instance Exception NoSuchColumn where

-- | This is similar to 'fieldWith' but instead of trying to
-- deserialize the field at the current position it goes through all
-- fields in the current row (starting at the beginning not the
-- current position) and tries to deserialize the first field with a
-- matching column name.
fieldByName :: FieldParser a -> ByteString {- ^ column name to look for -} -> RowParser a
fieldByName fieldP name =
  RP $ do
    Row {rowresult, row} <- ask
    ncols <- liftIO' (PQ.nfields rowresult)
    matchingCol <-
      liftIO' $
      findM
        (\col -> (Just name ==) <$> PQ.fname rowresult col)
        [PQ.Col 0 .. ncols - 1]
    case matchingCol of
      Nothing -> (lift . lift . conversionError) (NoSuchColumn name)
      Just col ->
        (lift . lift) $ do
          oid <- liftConversion (PQ.ftype rowresult col)
          val <- liftConversion (PQ.getvalue rowresult row col)
          fieldP (Field rowresult col oid) val

setToLastCol :: RowParser ()
setToLastCol =
  RP $ do
    Row {rowresult} <- ask
    ncols <- liftIO' (PQ.nfields rowresult)
    put ncols
