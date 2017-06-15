{-|
Module      : Database.PostgreSQL.Simple.FromRow.Named
Description : Generic implementation of FromRow based on record field names.
Copyright   : (c) Moritz Kiefer, 2017
License     : BSD-3
Maintainer  : moritz.kiefer@purelyfunctional.org

This module provides the machinery for implementing instances of
'FromRow' that deserialize based on the names of columns instead of
the positions of individual fields. This is particularly convenient
when deserializing to a Haskell record and you want the field names
and column names to match up. In this case 'gFromRow' can be used as
a generic implementation of 'fromRow'.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Database.PostgreSQL.Simple.FromRow.Named
  ( -- * Generic implementation of FromRow
    gFromRow
    -- * Deserialize individual fields based on their name
  , fieldByName
  , fieldByNameWith
    -- * Exception types
  , NoSuchColumn(..)
  , TooManyColumns(..)
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

npLength :: NP f xs -> Word
npLength xs = go 0 xs
  where
    go :: Word -> NP f xs -> Word
    go !i Nil = i
    go !i (_ :* xs') = go (i + 1) xs'

-- | Deserialize a type with a single record constructor by matching
-- the names of columns and record fields. Currently the complexity is /O(n^2)/ where n is the
-- number of record fields.
--
-- This is intended to be used as the implementation of 'fromRow'.
--
-- Throws
--
--   * 'NoSuchColumn' if there is a field for which there is no
--     column with the same name.
--
--   * 'TooManyColumns' if there more columns (counting both named
--     and unnamed columns) than record fields.
gFromRow :: forall a modName tyName constrName fields xs.
  ( Generic a
  , HasDatatypeInfo a
  , All2 FromField (Code a)
  , KnownSymbol modName
  , KnownSymbol tyName
  , DatatypeInfoOf a ~ 'T.ADT modName tyName '[ 'T.Record constrName fields]
  , Code a ~ '[xs]
  , T.DemoteFieldInfos fields xs
  ) => RowParser a
gFromRow = do
  let f :: forall f. FromField f => FieldInfo f -> RowParser f
      f (FieldInfo name) = fieldByName (BS.fromString name)
      fieldInfos :: NP FieldInfo xs
      fieldInfos = T.demoteFieldInfos (Proxy @fields)
  guardMatchingColumnNumber (npLength fieldInfos)
  res <-
    fmap (to . SOP . Z) $
    hsequence
      (hcliftA
         (Proxy :: Proxy FromField)
         f
         (T.demoteFieldInfos (Proxy :: Proxy fields)))
  setToLastCol
  pure res

guardMatchingColumnNumber :: Word -> RowParser ()
guardMatchingColumnNumber numFields =
  RP $ do
    Row {rowresult} <- ask
    PQ.Col (fromIntegral -> numCols) <- liftIO' (PQ.nfields rowresult)
    when
      (numCols /= numFields)
      ((lift . lift . conversionError) (TooManyColumns numFields numCols))

liftIO' :: IO a -> ReaderT Row (StateT PQ.Column Conversion) a
liftIO' = lift . lift . liftConversion

-- | Thrown when there is no column of the given name.
data NoSuchColumn =
  NoSuchColumn ByteString
  deriving (Show, Eq, Ord, Typeable)

instance Exception NoSuchColumn

-- | Thrown by 'gFromRow' when trying to deserialize to a record that
-- has less fields than the current row has columns (counting both
-- named and unnamed columns).
data TooManyColumns = TooManyColumns
  { numRecordFields :: !Word -- ^ The expected number of record fields.
  , numColumns :: !Word -- ^ The number of columns in the row that should have been deserialized.
  } deriving (Show, Eq, Ord, Typeable)

instance Exception TooManyColumns


-- | This is similar to 'fieldWith' but instead of trying to
-- deserialize the field at the current position it goes through all
-- fields in the current row (starting at the beginning not the
-- current position) and tries to deserialize the first field with a
-- matching column name.
fieldByNameWith :: FieldParser a -> ByteString {- ^ column name to look for -} -> RowParser a
fieldByNameWith fieldP name =
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

-- | This is a wrapper around 'fieldByNameWith' that gets the
-- 'FieldParser' via the typeclass instance. Take a look at the docs
-- for 'fieldByNameWith' for the details of this function.
fieldByName :: FromField a => ByteString {- ^ column name to look for -} -> RowParser a
fieldByName = fieldByNameWith fromField

setToLastCol :: RowParser ()
setToLastCol =
  RP $ do
    Row {rowresult} <- ask
    ncols <- liftIO' (PQ.nfields rowresult)
    put ncols
