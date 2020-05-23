module App.Database.Message where

import           RIO
import           Squeal.PostgreSQL

import           App.API
import           App.Database.Schema

-- insert :: (MonadPQ Schema m) => Message -> m ()
-- insert message = void $ executeParams insertQ message

-- insertQ :: Statement Schema Message ()
-- insertQ = manipulation $ insertInto_ #messages
--   (Values_ (Default `as` #id
--             :* Set (param @1) `as` #from
--             :* Set (param @2) `as` #email
--             :* Set (param @3) `as` #phone
--             :* Set (param @4) `as` #body
--            ))
