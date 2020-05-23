{-# LANGUAGE GADTs #-}
module App.API where

import           Data.Text                 (Text)
import qualified Generics.SOP              as SOP

import           Data.Aeson
import           Data.Aeson.GADT.TH
import           Data.Constraint.Extras.TH

data RequestG :: * -> * where
  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Int

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG

-- defineByDocumentFile "schema.graphql" [gql|
--   mutation SendMessage ($msg: Message!) {
--     sendMessage (message: $msg)
--   }
-- |]

-- instance SOP.Generic Message
-- instance SOP.HasDatatypeInfo Message
