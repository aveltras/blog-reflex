module App.Server where

import           Data.Aeson
import qualified Data.ByteString.Char8        as C8S
import qualified Data.ByteString.Lazy         as BL
import qualified Data.CaseInsensitive         as CI
import           Data.Constraint.Extras
import qualified Data.Map                     as Map
import           Data.Some
import qualified Data.Text.Encoding           as T
import           Network.HTTP.Req
import qualified Network.HTTP.Req             as Req
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp     as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip  (gzip)
import           Network.Wai.Middleware.Vhost (vhost)
import           Network.Wai.Static.TH        (mkStaticApp)
import           Reflex.Dom.Core              hiding (Error)
import           RIO
import qualified Sessionula                   as Session (defaultConfig, setup)
import           Sessionula.Backend.File
import qualified Sessionula.Extra             as Session
import qualified Sessionula.Frontend.Wai      as Session
import           Squeal.PostgreSQL
import           System.Environment

import           App.API
import           App.Database.Schema
import           App.Env
import           App.Frontend
import           App.Types
import           App.Web.Types

import           Lib.Iso
import           Lib.Source
import           Lib.View

mkStaticApp "static"


app :: Application
app request respond = do

  let headers = Prelude.filter ((==) (CI.mk "Cookie") . fst) $ requestHeaders request
      clientOptions = flip foldMap headers $ \(n, v) -> Req.header (CI.original n) v
      clientOptions' = Req.port 3000 <> clientOptions

      frontendConfig = Config ("http://static.blog.local:3000/" <> main_css) "http://jsaddle.blog.local:3000/jsaddle.js"

  (state, html) <- renderStatic' . runHydratableT $

    el "html" $ mdo
      el "head" $ do
        void $ headWidget frontendConfig headD
        injectIntoDOM $ constDyn frontendConfig
        injectIntoDOM $ (fmap . fmap) T.decodeUtf8 cacheD
      ((viewD, headD), cacheD) <- runSourceT Map.empty (reqXhrHandler clientOptions') gadtCodec $ runDynamicWriterT $ runViewT (constLocHandler $ T.decodeUtf8 . rawPathInfo $ request) $ el "body" bodyWidget
      pure viewD

  let status = case state of
        Right _ -> ok200
        Left _  -> status404

  respond $ responseLBS status [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict html


run :: (String -> Int -> IO [(Maybe String, Application)]) -> IO ()
run buildApps = do

  domain <- getEnv "APP_DOMAIN"
  webPort :: Int <- read <$> getEnv "APP_PORT"
  sessionsDir <- getEnv "APP_SESSIONS_DIR"
  dbConnStr <- C8S.pack <$> getEnv "DATABASE_URL"

  pool <- createConnectionPool dbConnStr 1 10 1
  let env = Env pool

  -- withConnection dbConnStr $ migrateDown migrations
  withConnection dbConnStr $ migrateUp migrations

  -- usingConnectionPool pool $ User.insert "romain.viallard@outlook.fr" "tac"

  manager <- Session.setup Session.defaultConfig =<< fileStorage sessionsDir

  let

    applyCors = cors $ const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ]
                                                              , corsOrigins = Just ([C8S.pack $ "http://" <> domain <> ":" <> show webPort], True)
                                                              } )

    sessionMiddleware = Session.middleware manager
                        Session.defaultSessionCookie { Session.setCookieSecure = False
                                                     , Session.setCookieDomain = Just (C8S.pack $ "." <> domain)
                                                     , Session.setCookieSameSite = Nothing }
                        Session.defaultCsrfSettings { Session.csrfExcludedMethods = [methodGet, methodPost] }


  otherApps <- (fmap . fmap) applyCors <$> buildApps domain webPort

  let

    vhosts = otherApps <> [ (Just "static", gzip def $ staticApp True)
                          -- , (Just "graphql", applyCors $ sessionMiddleware $ graphqlApp env)
                          , (Just "graphql", applyCors $ sessionMiddleware $ apiApp env)
                          , (Nothing, applyCors app)
                          ]

    vhostApp = vhosts <&> \(mSubdomain, waiApp) ->
      let domainToMatch = C8S.pack $ maybe domain (flip (<>) ("." <> domain)) mSubdomain <> ":" <> show webPort
      in ((==) (Just domainToMatch) . requestHeaderHost, waiApp)

  Warp.run webPort $ vhost vhostApp $ const $ flip ($) (responseLBS status503 [] "Service unavailable")

reqXhrHandler :: (PerformEvent t m, MonadIO (Performable m)) => Option 'Http -> Event t (Map Int WireFormat) -> m (Event t (Map Int WireFormat))
reqXhrHandler opts = performEvent . fmap toXhrRequest
  where
    toXhrRequest = traverse $ \wire -> runReq defaultHttpConfig $ do
      responseBody <$> req Req.POST -- method
                           (http "graphql.blog.local") -- safe by construction URL
                           (ReqBodyBs wire) -- use built-in options or add your own
                           bsResponse -- specify how to interpret response
                           opts -- query params, headers, explicit port number, etc.


handler :: RequestG a -> RIO Ctx (Either String a)
handler = fmap Right . \case

  SendMessage _msg -> pure True
  GetArticle slug -> undefined
  GetArticles -> undefined
  GetPage slug -> undefined

  Login _email _password -> do
    Session.authenticate $ UserId 5
    pure True

  Logout -> Session.logout

  CreateArticle _ -> undefined
  UpdateArticle _ _ -> undefined
  CreatePage _ -> undefined
  UpdatePage _ _ -> undefined

apiApp :: Env -> Application
apiApp env request respond = do
  bs <- lazyRequestBody request
  case decode' bs of
    Nothing -> respond $ responseLBS status400 [ (hContentType, "application/json") ] ""
    Just (Some appRequest) -> do
      resp <- runRIO (Ctx env $ Session.extractSession request) $ handler appRequest
      respond $ responseLBS ok200 [ (hContentType, "application/json") ] $ has @ToJSON appRequest $ encode $ resp
