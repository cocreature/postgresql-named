# postgresql-named

[![Travis](https://img.shields.io/travis/cocreature/postgresql-named.svg)](https://travis-ci.org/cocreature/postgresql-named)
[![Hackage](https://img.shields.io/hackage/v/postgresql-named.svg)](https://hackage.haskell.org/package/postgresql-named)

Library for deserializing rows in `postgresql-simple` (or any other
library that uses `FromRow`) based on column names instead of the
positions of columns.

## Example

```haskell
{-# LANGUAGE DeriveGeneric #-}
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.FromRow.Named
import qualified GHC.Generics as GHC
import           Generics.SOP

data Foobar = Foobar
  { foo :: !String
  , bar :: !Int
  } deriving (Show, Eq, Ord, GHC.Generic)


instance Generic Foobar

instance HasDatatypeInfo Foobar

instance FromRow Foobar where
  fromRow = gFromRow
```
