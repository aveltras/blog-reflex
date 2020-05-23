{-# LANGUAGE DeriveAnyClass #-}

module App.Types where

import           Data.Aeson
import           Data.Serialize (Serialize)
import qualified Generics.SOP   as SOP
import           RIO

newtype ArticleId = ArticleId Int32
  deriving newtype (FromJSON, ToJSON)

data Article = Article
  { articleTitle :: Text
  , articleSlug  :: Text
  , articleBody  :: Text
  } deriving stock (Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON)

newtype MessageId = MessageId Int32
  deriving newtype (FromJSON, ToJSON)

data Message = Message
  { messageFrom  :: Text
  , messageEmail :: Text
  , messagePhone :: Maybe Text
  , messageBody  :: Text
  } deriving stock (Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON)

newtype PageId = PageId Int32
  deriving newtype (FromJSON, ToJSON)

data Page = Page
  { pageTitle :: Text
  , pageSlug  :: Text
  , pageBody  :: Text
  } deriving stock (Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON)

newtype UserId = UserId Int32
  deriving newtype (Serialize)

data User = User
  { userEmail :: Text
  } deriving stock (Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON)
