{-# LANGUAGE BangPatterns, DataKinds, DeriveGeneric, FlexibleContexts, GADTs, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeOperators #-}
module Lib
  ( fieldByName
  , deserialize
  ) where

import           Control.Monad.Extra
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BS
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Internal
import           GHC.TypeLits
import           Generics.SOP
import qualified Generics.SOP.Type.Metadata as T

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
  let f
        :: forall f.
           FromField f
        => FieldInfo f -> RowParser f
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

fieldByName :: FieldParser a -> ByteString -> RowParser a
fieldByName fieldP name =
  RP $ do
    Row {..} <- ask
    ncols <- lift (lift (liftConversion (PQ.nfields rowresult)))
    matchingCol <-
      (lift . lift . liftConversion) $
      findM
        (\col -> (Just name ==) <$> PQ.fname rowresult col)
        [PQ.Col 0 .. ncols - 1]
    case matchingCol of
      Nothing -> error "no such column"
      Just col ->
        (lift . lift) $ do
          oid <- liftConversion (PQ.ftype rowresult col)
          val <- liftConversion (PQ.getvalue rowresult row col)
          fieldP (Field rowresult col oid) val

setToLastCol :: RowParser ()
setToLastCol =
  RP $ do
    Row {..} <- ask
    ncols <- (lift . lift . liftConversion) (PQ.nfields rowresult)
    put ncols
