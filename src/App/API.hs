{-# LANGUAGE GADTs #-}

module App.API where

import           App.Types
import           Data.Aeson
import           Data.Aeson.GADT.TH
import           Data.Constraint.Extras.TH
import           Data.Text

data RequestG :: * -> * where

  -- Public API
  SendMessage :: Message -> RequestG Bool
  GetArticle :: Text -> RequestG (Maybe Article)
  GetArticles :: RequestG [Article]
  GetPage :: Text -> RequestG (Maybe Page)
  Login :: Text -> Text -> RequestG Bool

  -- Protected API
  Logout :: RequestG ()
  CreateArticle :: Article -> RequestG Int
  UpdateArticle :: ArticleId -> Article -> RequestG Int
  CreatePage :: Page -> RequestG Int
  UpdatePage :: PageId -> Page -> RequestG Int

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG
