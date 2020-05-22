module App.Server where

import           Data.Aeson
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.ByteString.Lazy.Char8             as C8
import qualified Data.CaseInsensitive                   as CI
import qualified Data.Map                               as Map
import           Data.Morpheus                          (interpreter)
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import           Language.Javascript.JSaddle            (syncPoint)
import qualified Language.Javascript.JSaddle.WebSockets as JW
import qualified Network.HTTP.Req                       as Req
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp               as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip            (gzip)
import           Network.Wai.Middleware.Vhost           (vhost)
import           Network.Wai.Static.TH                  (mkStaticApp)
import           Network.WebSockets                     (defaultConnectionOptions)
import           Reflex.Dom.Core
import           RIO
import qualified Sessionula                             as Session (defaultConfig,
                                                                    setup)
import           Sessionula.Backend.File
import qualified Sessionula.Frontend.Wai                as Session

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

placeholder = "%%%"

app :: Application
app request respond = do

  let headers = Prelude.filter ((==) (CI.mk "Cookie") . fst) $ requestHeaders request
      clientOptions = flip foldMap headers $ \(n, v) -> Req.header (CI.original n) v
      clientOptions' = Req.port 3000 <> clientOptions

  let commentedPlaceholder = "<!--" <> placeholder <> "-->"

  ((state, cache), html) <- renderStatic' . runHydratableT $

    el "html" $ mdo
      el "head" $ void (headWidget headD) >> (comment $ T.decodeUtf8 placeholder)
      ((viewD, headD), cacheB) <- runSourceT Map.empty (reqXhrHandler clientOptions') graphqlCodec $ runDynamicWriterT $ runViewT (constLocHandler $ T.decodeUtf8 . rawPathInfo $ request) $ el "body" bodyWidget
      pure (viewD, cacheB)

  let status = case state of
        Right _ -> ok200
        Left _  -> status404

  let (a, b) = BS.breakSubstring commentedPlaceholder html
      prerenderedHtml = a <> "<script data-prerenderblob>//" <> (BL.toStrict . encode $ T.decodeUtf8 <$> cache) <> "</script>" <> BS.drop (BS.length commentedPlaceholder) b

  respond $ responseLBS status [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict prerenderedHtml


run :: IO ()
run = do
  let port = 3000

  manager <- Session.setup Session.defaultConfig =<< fileStorage "/tmp/sessions"


  jsApp <- JW.jsaddleOr defaultConnectionOptions (mainJS >> syncPoint) $ JW.jsaddleAppWithJs $ JW.jsaddleJs' (Just $ "http://jsaddle.blog.local:" <> (C8.pack . show) port) False
  Warp.run port $ vhost
    [ ((==) (Just . T.encodeUtf8 . T.pack . (<>) "static.blog.local:" $ show port) . requestHeaderHost, gzip def $ staticApp True)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "jsaddle.blog.local:" $ show port) . requestHeaderHost, cors (const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] } )) $ jsApp)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "graphql.blog.local:" $ show port) . requestHeaderHost, laxCors $ sessionMiddleware manager $ graphqlApp)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "blog.local:" $ show port) . requestHeaderHost, laxCors app)
    ] $ const $ flip ($) (responseLBS status503 [] "Service unavailable")

    where
      laxCors = cors $ const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ]
                                                              , corsOrigins = Just (["http://blog.local:3000"], True)
                                                             } )

      sessionMiddleware man =
        Session.middleware
        man
        Session.defaultSessionCookie { Session.setCookieSecure = False
                                     , Session.setCookieDomain = Just ".blog.local"
                                     , Session.setCookieSameSite = Nothing }
        Session.defaultCsrfSettings { Session.csrfExcludedMethods = [methodGet, methodPost] }






