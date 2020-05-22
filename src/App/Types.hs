module App.Types where

import           Data.Serialize (Serialize)
import           RIO

newtype UserId = UserId Int32
  deriving (Serialize)
