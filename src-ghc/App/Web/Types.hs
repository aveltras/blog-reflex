module App.Web.Types where

import           RIO
import qualified Sessionula          as Session (Handle)
import qualified Sessionula.Extra    as Session

import           App.Database
import           App.Database.Schema
import           App.Env
import           App.Types

data Ctx = Ctx
  { ctxEnv     :: Env
  , ctxSession :: Session.Handle UserId
  }

instance Session.HasSession Ctx where
  type Auth Ctx = UserId
  sessionL = lens ctxSession (\x y -> x { ctxSession = y })

instance (db ~ Schema) => HasPool Ctx db where
  poolL = lens (envPool . ctxEnv) (\x y -> x { ctxEnv = (ctxEnv x) { envPool = y } })
