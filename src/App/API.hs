{-# LANGUAGE GADTs #-}

module App.API where

import           App.Types
import           Data.Aeson
import           Data.Aeson.GADT.TH
import           Data.Constraint.Extras.TH

data RequestG :: * -> * where
  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Int
  SendMessage :: Message -> RequestG Bool

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG
