{-# LANGUAGE DeriveAnyClass #-}

module App.Types where

import           Data.Aeson
import           Data.Serialize (Serialize)
import qualified Generics.SOP   as SOP
import           RIO

newtype UserId = UserId Int32
  deriving newtype (Serialize)

data Message = Message
  { messageFrom  :: Text
  , messageEmail :: Text
  , messagePhone :: Maybe Text
  , messageBody  :: Text
  } deriving stock (Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON)
