{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import           Control.Lens
import           Control.Monad                          (forM_, void)
import           Control.Monad.Fix                      (MonadFix)
import           Control.Monad.IO.Class                 (MonadIO, liftIO)
import           Control.Monad.Ref
import           Data.Aeson
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Builder                as B
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.ByteString.Lazy.Char8             as C8
import           Data.Dependent.Sum                     (DSum (..))
import           Data.Functor.Identity                  (Identity (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import           Language.Javascript.JSaddle            (JSM, MonadJSM,
                                                         syncPoint)
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
import           UnliftIO.Concurrent
import           Web.PathPieces

import           Data.Morpheus                          (interpreter)
import           Data.Morpheus.Client
import           Data.Morpheus.Document
import           Data.Morpheus.Types                    (GQLRootResolver (..),
                                                         ResolverQ,
                                                         Undefined (..))


import           Source
import           View

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
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "localhost:" $ show port) . requestHeaderHost, app)
    ] $ const $ flip ($) (responseLBS status503 [] "Service unavailable")

app :: Application
app request respond = do

  (state, html) <- renderStatic' . runHydratableT $
    el "html" $ do
      el "head" $ do
        elAttr "script" ("src" =: "http://jsaddle.localhost:3000/jsaddle.js") blank
        elAttr "link" ("rel" =: "stylesheet" <> "href" =: ("http://static.localhost:3000/" <> test_css)) blank
      el "body" $ do
        text "hello"
        clickE <- button "click"
        textD <- holdDyn "before click" $ "afterClick "<$ clickE
        dynText textD
        runViewT (constLocHandler $ T.decodeUtf8 . rawPathInfo $ request) appW

  let status = case state of
        Right _ -> ok200
        Left _  -> status404

  respond $ responseLBS status [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict html

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

appW :: (DomBuilder t m, HasView t View ViewError m, PerformEvent t m, Prerender js t m, PostBuild t m) => m ()
appW = do
  viewD <- askView
  void $ dyn $ (\case
                   Right v -> case v of
                     HomeV    -> do
                       text "home"
                       linkTo ContactV $ text "go contact"
                       prerender_ blank graphQLwidget
                     ContactV -> do
                       text "contact"
                       linkTo HomeV $ text "go home"
                   Left e -> case e of
                     ViewError -> do
                       text "not found"
                       linkTo HomeV $ text "go home"
                       linkTo ContactV $ text "go contact"
               ) <$> viewD

graphQLwidget :: (PostBuild t m, MonadHold t m, DomBuilder t m, XhrConstraints t m) => m ()
graphQLwidget = do
  clickE <- button "click"
  responseE :: Event t (Either String GetDeity) <- xhrQuery (GetDeityArgs "tac" <$ clickE)
  responseD <- holdDyn "" $ ffor responseE $ \r -> case r of
    Left s  -> T.pack $ "Error ---->" <> s
    Right g -> T.pack $ "Success ---> " <> show g
  display responseD
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

type XhrConstraints t m = (HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m, HasJSContext m, MonadIO m, MonadJSM m)

xhrQuery :: forall query m t. (XhrConstraints t m, FromJSON query, Fetch query) => Event t (Args query) -> m (Event t (Either String query))
xhrQuery queryE = performEvent $ toPerformable <$> queryE

  where

    toPerformable :: Args query -> Performable m (Either String query)
    toPerformable args = fetch xhrFetch args

    xhrFetch queryBS = do

      let req = xhrRequest "POST" "http://graphql.localhost:3000" $ def & xhrRequestConfig_sendData .~ BL.toStrict queryBS

      resultVar <- newEmptyMVar
      void $ newXMLHttpRequest req $ liftIO . putMVar resultVar
      resp <- takeMVar resultVar

      let body = case resp ^. xhrResponse_responseText of
            Nothing  -> error "boom"
            Just txt -> BL.fromStrict . T.encodeUtf8 $ txt

      pure body
