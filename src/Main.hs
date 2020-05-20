{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Control.Monad                          (forM_, join, void)
import           Control.Monad.IO.Class                 (MonadIO)
import           Control.Monad.Ref
import           Data.Aeson
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Builder                as B
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.ByteString.Lazy.Char8             as C8
import           Data.Dependent.Sum                     (DSum (..))
import           Data.Functor.Identity                  (Identity (..))
import           Data.Map
import qualified Data.Map                               as Map
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import           GHC.IORef
import           Language.Javascript.JSaddle            (JSM, eval, syncPoint)
import qualified Language.Javascript.JSaddle.WebSockets as JW
import           Network.HTTP.Types                     hiding (Query)
import           Network.Wai
import qualified Network.Wai.Handler.Warp               as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip            (gzip)
import           Network.Wai.Middleware.Vhost           (vhost)
import           Network.Wai.Static.TH                  (mkStaticApp)
import           Network.WebSockets                     (defaultConnectionOptions)
import           Reflex.Dom.Core                        hiding (Query)
import           Reflex.Dom.Main                        as Main
import           Reflex.Host.Class
import           Web.PathPieces

import           Data.Morpheus                          (interpreter)
import           Data.Morpheus.Client
import           Data.Morpheus.Document
import           Data.Morpheus.Types                    (GQLRootResolver (..),
                                                         Undefined (..))


import           Source
import           View

import qualified GHCJS.DOM                              as DOM
import qualified GHCJS.DOM.Document                     as DOM
import qualified GHCJS.DOM.HTMLScriptElement            as DOM
import qualified GHCJS.DOM.ParentNode                   as DOM
import qualified GHCJS.DOM.Types                        as DOM

mkStaticApp "static"

importGQLDocumentWithNamespace "schema.graphql"

defineByDocumentFile
  "schema.graphql"
  [gql|
    query GetDeity ($goName: String!)
    {
      deity (name: $goName)
      { power }
    }
  |]

main :: IO ()
main = do
  let port = 3000
  jsApp <- JW.jsaddleOr defaultConnectionOptions (mainJS >> syncPoint) $ JW.jsaddleAppWithJs $ JW.jsaddleJs' (Just $ "http://jsaddle.localhost:" <> (C8.pack . show) port) False
  Warp.run port $ vhost
    [ ((==) (Just . T.encodeUtf8 . T.pack . (<>) "static.localhost:" $ show port) . requestHeaderHost, gzip def $ staticApp True)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "jsaddle.localhost:" $ show port) . requestHeaderHost, cors (const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] } )) $ jsApp)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "graphql.localhost:" $ show port) . requestHeaderHost, simpleCors graphqlApp)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "api.localhost:" $ show port) . requestHeaderHost, cors (const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] } )) $ echoApp)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "localhost:" $ show port) . requestHeaderHost, app)
    ] $ const $ flip ($) (responseLBS status503 [] "Service unavailable")

echoApp :: Application
echoApp request respond = do
  -- num :: Int <- randomRIO (1, 10)
  lbs <- lazyRequestBody request
  respond $ responseLBS ok200 [(hContentType, "application/json")] lbs

placeholder = "%%%"

app :: Application
app request respond = do

  cacheRef <- newIORef Map.empty

  let commentedPlaceholder = "<!--" <> placeholder <> "-->"

  -- (state, html) <- renderStatic' . runHydratableT . runSourceT ("http://graphql.localhost:3000", constant Map.empty) $
  (state, html) <- renderStatic' . runHydratableT . runSourceT (reqXhrHandler cacheRef) graphqlCodec . runViewT (constLocHandler $ T.decodeUtf8 . rawPathInfo $ request) $
    el "html" $ do
      el "head" $ void headWidget
      el "body" bodyWidget

  let status = case state of
        Right _ -> ok200
        Left _  -> status404

  cache <- readIORef cacheRef

  let (a, b) = BS.breakSubstring commentedPlaceholder html
      prerenderedHtml = a <> "<script data-prerenderblob>//" <> (BL.toStrict . encode $ T.decodeUtf8 <$> cache) <> "</script>" <> BS.drop (BS.length commentedPlaceholder) b

  respond $ responseLBS status [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict prerenderedHtml
  -- respond $ responseLBS status [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict html


headWidget :: (DomBuilder t m) => m ()
headWidget = do
  elAttr "script" ("src" =: "http://jsaddle.localhost:3000/jsaddle.js") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: ("http://static.localhost:3000/" <> test_css)) blank
  comment $ T.decodeUtf8 placeholder

bodyWidget :: (MonadIO (Performable m), Prerender js t m, PostBuild t m, HasSource t IsGraphQLQuery m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilder t m, HasView t View ViewError m) => m ()
bodyWidget = appW
--do
  -- text "hello"
  -- buildE <- getPostBuild
  -- textD <- holdDyn "before click" $ "afterClick " <$ buildE
  -- dynText textD
  --appW

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

-- appW :: (DomBuilder t m, MonadHold t m, HasSource t js m, HasView t View ViewError m, PerformEvent t m, Prerender js t m, PostBuild t m) => m ()
appW :: (MonadIO (Performable m), DomBuilder t m, TriggerEvent t m, HasSource t IsGraphQLQuery m, MonadHold t m, HasView t View ViewError m, PerformEvent t m, Prerender js t m, PostBuild t m) => m ()
appW = do
  viewD <- askView
  void $ dyn $ (\case
                   Right v -> case v of
                     HomeV    -> do
                       text "home"
                       linkTo ContactV $ text "go contact"
                       graphQLwidget
                     ContactV -> do
                       -- text "contact"
                       linkTo HomeV $ text "go home"
                   Left e -> case e of
                     ViewError -> do
                       -- text "not found"
                       linkTo HomeV $ text "go home"
                       linkTo ContactV $ text "go contact"
               ) <$> viewD


mainJS :: JSM ()
mainJS = do

  Just doc <- DOM.currentDocument
  Just hd <- DOM.getHead doc
  t <- DOM.querySelector hd ("[data-prerenderblob]" :: Text) >>= \case
    Nothing -> pure ""
    Just node ->
      DOM.castTo DOM.HTMLScriptElement node >>= \case
        Nothing -> pure ""
        Just e -> T.drop 2 <$> (DOM.getText e) :: JSM Text


  let cacheMap = decode' @(Map Int Text) $ BL.fromStrict $ T.encodeUtf8 t

      cacheMap' = maybe Map.empty (fmap T.encodeUtf8) cacheMap -- doesn't work
      -- cacheMap' = Map.empty -- works

  -- eval ("console.log('"<> show t <>"')" :: String)
  eval ("console.log('"<> show cacheMap' <>"')" :: String)



  let
    bodyWidget' = do
      _ <- runSourceT (reflexXhrHandler def (constant cacheMap')) graphqlCodec $ runViewT browserLocHandler appW
      blank

    w :: (FrontendWidget () -> TriggerEventT DomTimeline (DomCoreWidget ()) x)
      -> (FrontendWidget () -> TriggerEventT DomTimeline (DomCoreWidget ()) x)
      -> TriggerEventT DomTimeline (DomCoreWidget ()) x
    w appendHead appendBody = appendHead headWidget >> appendBody bodyWidget'


  runHydrationWidgetWithHeadAndBody (pure ()) w
    -- $ do
  -- Main.mainWidget $ do
    -- buildE <- getPostBuild
    -- clickE <- button "click"
    -- textD <- holdDyn "before click" $ "afterClick " <$ leftmost [buildE, clickE]
    -- dynText textD
    -- _ <- runSourceT (reflexXhrHandler def (constant cacheMap')) graphqlCodec $ runViewT browserLocHandler appW
    -- blank

type FrontendWidget = HydrationDomBuilderT HydrationDomSpace DomTimeline (DomCoreWidget ())
-- runFrontend :: forall a. FromJSON a => FrontendRunner () a -> JSM ()
-- runFrontend frontendRunner = do

--   Just doc <- currentDocument
--   Just body <- getBody doc
--   dataset <- getDataset body
--   encodedCfg <- get dataset ("ealeCfg" :: Text)

--   let Right b64 = (Base64.decode . LBS.fromStrict . encodeUtf8) encodedCfg
--       config = fromMaybe (error "could not retrieve configuration from DOM") (decode @a b64)
-- -- decodeUtf8 . toStrict . Base64.encode . encode $ config
--   let (headWidget, bodyWidget) = frontendRunner config

--       w :: (FrontendWidget () -> TriggerEventT DomTimeline (DomCoreWidget ()) x)
--         -> (FrontendWidget () -> TriggerEventT DomTimeline (DomCoreWidget ()) x)
--         -> TriggerEventT DomTimeline (DomCoreWidget ()) x
--       w appendHead appendBody = appendHead headWidget >> appendBody bodyWidget

--   runHydrationWidgetWithHeadAndBody (pure ()) w



graphQLwidget :: (MonadIO (Performable m), Prerender js t m, HasSource t IsGraphQLQuery m, TriggerEvent t m, PostBuild t m, MonadHold t m, PerformEvent t m, DomBuilder t m) => m ()
graphQLwidget = do

  buildE <- getPostBuild
  responseE :: Event t (Either String GetDeity) <- requesting $ (IsGraphQLQuery (GetDeityArgs "tac")) <$ (leftmost [traceEvent "server - build event" buildE])
  widgetD <- holdDyn "initial" $ ffor (traceEvent "server widget" responseE) $ \r -> case r of
    Left s  -> T.pack $ "Error ---->" <> s
    Right g -> T.pack $ "Success ---> " <> show g

  prerender_ (el "div" $ dynText widgetD) (el "div" $ dynText widgetD)

  blank



{-# INLINE renderStatic' #-}
renderStatic' :: StaticWidget x (Dynamic DomTimeline a) -> IO (a, BS.ByteString)
renderStatic' w = do
  runDomHost $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    nextRunWithReplaceKey <- newRef 0
    let env0 = StaticDomBuilderEnv True Nothing nextRunWithReplaceKey
    ((res, bs), FireCommand fire) <- hostPerformEventT $ runStaticDomBuilderT (runPostBuildT w postBuild) env0
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    bs' <- sample bs
    a <- sample . current $ res
    return (a, BL.toStrict $ B.toLazyByteString bs')

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    {
      queryResolver = Query {queryDeity},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    queryDeity QueryDeityArgs {queryDeityArgsName} = pure Deity
      {
        deityName = pure "Morpheus"
      , deityPower = pure (Just "Shapeshifting")
      }

api :: BL.ByteString -> IO BL.ByteString
api = interpreter rootResolver

graphqlApp :: Application
graphqlApp request respond = do
  bs <- strictRequestBody request
  resp <- api bs
  respond $ responseLBS ok200 [(hContentType, "application/json")] resp
