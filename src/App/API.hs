module App.API where

import           Data.Morpheus.Client
import           Data.Text            (Text)

defineByDocumentFile "schema.graphql" [gql|
  query GetDeity ($goName: String!) {
    deity (name: $goName) {
      name
      power
    }
  }
|]
