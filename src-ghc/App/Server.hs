module App.Server where

import qualified Data.ByteString.Char8        as C8S
import qualified Data.ByteString.Lazy         as BL
import qualified Data.CaseInsensitive         as CI
import qualified Data.Map                     as Map
import           Data.Morpheus                (interpreter)
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
import           Reflex.Dom.Core
import           RIO
import qualified Sessionula                   as Session (defaultConfig, setup)
import           Sessionula.Backend.File
import qualified Sessionula.Frontend.Wai      as Session
import           System.Environment

import           App.Frontend
import           App.Web.GraphQL
import           App.Web.Types

import           Lib.Iso
import           Lib.Source
import           Lib.View

mkStaticApp "static"

graphqlApp :: Application
graphqlApp request respond = do
  let sessionHandle = Session.extractSession request
  bs <- lazyRequestBody request
  resp <- runRIO (Ctx sessionHandle) $ interpreter rootResolver bs
  respond $ responseLBS ok200 [ (hContentType, "application/json") ] resp

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
      ((viewD, headD), cacheD) <- runSourceT Map.empty (reqXhrHandler clientOptions') graphqlCodec $ runDynamicWriterT $ runViewT (constLocHandler $ T.decodeUtf8 . rawPathInfo $ request) $ el "body" bodyWidget
      pure viewD

  let status = case state of
        Right _ -> ok200
        Left _  -> status404

  respond $ responseLBS status [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict html


run :: (String -> Int -> IO [(Maybe String, Application)]) -> IO ()
run buildApps = do

  domain <- getEnv "APP_DOMAIN"
  port :: Int <- read <$> getEnv "APP_PORT"
  sessionsDir <- getEnv "APP_SESSIONS_DIR"

  manager <- Session.setup Session.defaultConfig =<< fileStorage sessionsDir

  let

    applyCors = cors $ const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ]
                                                              , corsOrigins = Just ([C8S.pack $ "http://" <> domain <> ":" <> show port], True)
                                                              } )

    sessionMiddleware = Session.middleware manager
                        Session.defaultSessionCookie { Session.setCookieSecure = False
                                                     , Session.setCookieDomain = Just (C8S.pack $ "." <> domain)
                                                     , Session.setCookieSameSite = Nothing }
                        Session.defaultCsrfSettings { Session.csrfExcludedMethods = [methodGet, methodPost] }


  otherApps <- (fmap . fmap) applyCors <$> buildApps domain port

  let

    vhosts = otherApps <> [ (Just "static", gzip def $ staticApp True)
                          , (Just "graphql", applyCors $ sessionMiddleware graphqlApp)
                          , (Nothing, applyCors app)
                          ]

    vhostApp = vhosts <&> \(mSubdomain, waiApp) ->
      let domainToMatch = C8S.pack $ maybe domain (flip (<>) ("." <> domain)) mSubdomain <> ":" <> show port
      in ((==) (Just domainToMatch) . requestHeaderHost, waiApp)

  Warp.run port $ vhost vhostApp $ const $ flip ($) (responseLBS status503 [] "Service unavailable")

reqXhrHandler :: (PerformEvent t m, MonadIO (Performable m)) => Option 'Http -> Event t (Map Int WireFormat) -> m (Event t (Map Int WireFormat))
reqXhrHandler opts = performEvent . fmap toXhrRequest
  where
    toXhrRequest = traverse $ \wire -> runReq defaultHttpConfig $ do
      responseBody <$> req Req.POST -- method
                           (http "graphql.blog.local") -- safe by construction URL
                           (ReqBodyBs wire) -- use built-in options or add your own
                           bsResponse -- specify how to interpret response
                           opts -- query params, headers, explicit port number, etc.
