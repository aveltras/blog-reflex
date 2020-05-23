module App.Env where

import           App.Database
import           App.Database.Schema
import           RIO

data Env = Env
  { envPool       :: DbPool Schema
  }

instance (db ~ Schema) => HasPool Env db where
  poolL = lens envPool (\x y -> x { envPool = y })
