{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Main where

import           Control.Monad                          (void)
import           Control.Monad.Fix                      (MonadFix)
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
import           Network.Wai.Static.TH                  (mkStaticApp)
import           Network.WebSockets                     (defaultConnectionOptions)
import           Reflex.Dom.Core
import           Reflex.Dom.Main                        as Main
import           Web.PathPieces

import           Source
import           View

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
        void $ runViewT (constLocHandler "") appW

  respond $ responseLBS ok200 [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict html

mainJS :: JSM ()
mainJS = Main.mainWidget $ do
  text "hello"
  clickE <- button "click"
  textD <- holdDyn "before click" $ "afterClick "<$ clickE
  dynText textD
  _ <- runViewT browserLocHandler appW
  blank


data View = HomeV
          | ContactV

instance PathPiece View where
  fromPathPiece = \case
    "" -> Just HomeV
    "contact" -> Just ContactV
    _ -> Nothing
  toPathPiece = \case
    HomeV -> ""
    ContactV -> "contact"

appW :: (DomBuilder t m, HasView t View ViewError m, PerformEvent t m, PostBuild t m) => m ()
appW = do
  viewD <- askView
  void $ dyn $ (\case
                   Right v -> case v of
                     HomeV    -> do
                       text "home"
                       linkTo ContactV $ text "go contact"
                     ContactV -> do
                       text "contact"
                       linkTo HomeV $ text "go home"
                   Left e -> case e of
                     ViewError -> do
                       text "not found"
                       linkTo HomeV $ text "go home"
                       linkTo ContactV $ text "go contact"
               ) <$> viewD
