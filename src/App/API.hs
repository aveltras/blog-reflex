{-# LANGUAGE GADTs #-}

module App.API where

import           App.Types
import           Data.Aeson
import           Data.Aeson.GADT.TH
import           Data.Constraint.Extras.TH
import           Data.Text

data RequestG :: * -> * where

  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Int

  -- Public API
  SendMessage :: Message -> RequestG Bool
  GetArticle :: RequestG Text
  GetArticles :: RequestG Text
  GetPage :: RequestG Text
  Login :: RequestG Text

  -- Protected API
  Logout :: RequestG Text
  CreateArticle :: RequestG Int
  UpdateArticle :: RequestG Int
  CreatePage :: RequestG Int
  UpdatePage :: RequestG Int

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG
