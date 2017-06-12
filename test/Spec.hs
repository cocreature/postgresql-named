{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import           Test.Hspec

import           Control.Exception
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import qualified GHC.Generics as GHC
import           Generics.SOP
import           Lib

data Foobar = Foobar
  { foo :: !String
  , bar :: !Int
  } deriving (Show, Eq, Ord, GHC.Generic)


instance Generic Foobar

instance HasDatatypeInfo Foobar


instance FromRow Foobar where
  fromRow = deserialize

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection =
  bracket
    (connectPostgreSQL "host=localhost port=5432 user=postgres dbname=postgres")
    close

main :: IO ()
main =
  hspec $ do
    around withDatabaseConnection $ do
      describe "deserialize" $ do
        it "deserializes (foo, bar) correctly" $ \conn -> do
          query_ conn "select 'abc'::text as foo, 1 as bar" `shouldReturn`
            [Foobar "abc" 1]
        it "deserializes (bar, foo) correctly" $ \conn -> do
          query_ conn "select 1 as bar, 'abc'::text as foo" `shouldReturn`
            [Foobar "abc" 1]
