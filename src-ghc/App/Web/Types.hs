module App.Web.Types where

import           RIO
import qualified Sessionula       as Session (Handle)
import qualified Sessionula.Extra as Session

import           App.Env
import           App.Types

data Ctx = Ctx
  { ctxEnv     :: Env
  , ctxSession :: Session.Handle UserId
  }

instance Session.HasSession Ctx where
  type Auth Ctx = UserId
  sessionL = lens ctxSession (\x y -> x { ctxSession = y })
