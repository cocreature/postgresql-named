{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}
module Lib
  ( fieldByName
  ) where
import           Database.PostgreSQL.Simple.Internal
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.LibPQ as PQ
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Control.Monad.Extra

data Foobar = Foobar
  { foo :: !String
  , bar :: !Int
  } deriving (Show, Eq, Ord)

fieldByName :: FieldParser a -> ByteString -> RowParser a
fieldByName fieldP name =
  RP $ do
    r@Row {..} <- ask
    ncols <- lift (lift (liftConversion (PQ.nfields rowresult)))
    matchingCol <-
      (lift . lift . liftConversion) $
      findM
        (\col -> (Just name ==) <$> PQ.fname rowresult col)
        [PQ.Col 0 .. ncols - 1]
    case matchingCol of
      Nothing -> error "no such column"
      Just col -> (lift . lift) $ do
        liftConversion (print col)
        oid <- liftConversion (PQ.ftype rowresult col)
        val <- liftConversion (PQ.getvalue rowresult row col)
        fieldP (Field rowresult col oid) val

setToLastCol :: RowParser ()
setToLastCol =
  RP $ do
    Row {..} <- ask
    ncols <- (lift . lift . liftConversion) (PQ.nfields rowresult)
    put ncols

instance FromRow Foobar where
  fromRow = do
    foo <- fieldByName fromField "foo"
    bar <- fieldByName fromField "bar"
    setToLastCol
    pure (Foobar foo bar)

test :: IO ()
test = do
  conn <-
    connectPostgreSQL "host=localhost port=5432 user=postgres dbname=postgres"
  [res] <- query_ conn "select 1 as bar, 'abc'::text as foo"
  print (res :: Foobar)
  pure ()
