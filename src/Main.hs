module Main where

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.ByteString.Lazy.Char8             as C8
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import           Language.Javascript.JSaddle            (JSM, syncPoint)
import qualified Language.Javascript.JSaddle.WebSockets as JW
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp               as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip            (gzip)
import           Network.Wai.Middleware.Vhost           (vhost)
import           Network.Wai.Static.TH
import           Network.WebSockets
import           Reflex.Dom
import           Reflex.Dom.Main                        as Main

mkStaticApp "static"

main :: IO ()
main = do
  let port = 3000
  jsApp <- JW.jsaddleOr defaultConnectionOptions (mainJS >> syncPoint) $ JW.jsaddleAppWithJs $ JW.jsaddleJs' (Just $ "http://jsaddle.localhost:" <> (C8.pack . show) port) False
  Warp.run port $ vhost
    [ ((==) (Just . T.encodeUtf8 . T.pack . (<>) "static.localhost:" $ show port) . requestHeaderHost, gzip def $ staticApp True)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "jsaddle.localhost:" $ show port) . requestHeaderHost, cors (const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] } )) $ jsApp)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "localhost:" $ show port) . requestHeaderHost, app)
    ] $ const $ flip ($) (responseLBS status503 [] "Service unavailable")

app :: Application
app _request respond = do
  (_, html) <- renderStatic $
    el "html" $ do
      el "head" $ do
        elAttr "script" ("src" =: "http://jsaddle.localhost:3000/jsaddle.js") blank
        elAttr "link" ("rel" =: "stylesheet" <> "href" =: ("http://static.localhost:3000/" <> test_css)) blank
      el "body" $ do
        text "hello"
        clickE <- button "click"
        textD <- holdDyn "before click" $ "afterClick "<$ clickE
        dynText textD

  respond $ responseLBS ok200 [("Content-Type", "text/html")] $ "<!doctype html>" <> BL.fromStrict html

mainJS :: JSM ()
mainJS = Main.mainWidget $ do
  text "hello"
  clickE <- button "click"
  textD <- holdDyn "before click" $ "afterClick "<$ clickE
  dynText textD

widget :: Widget x ()
widget = do
  text "hello"
  clickE <- button "click"
  textD <- holdDyn "before click" $ "afterClick "<$ clickE
  dynText textD

