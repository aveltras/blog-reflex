{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Control.Lens
import           Control.Monad                          (forM_, void, (<=<))
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
import           Data.Map
import qualified Data.Map                               as Map
import           Data.Maybe                             (isJust)
import           Data.Monoid                            (Sum (..), getSum)
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
import           System.Random
import           UnliftIO.Concurrent
import           Web.PathPieces

import           Data.Hashable
import           Data.Morpheus                          (interpreter)
import           Data.Morpheus.Client
import           Data.Morpheus.Document
import           Data.Morpheus.Types                    (GQLRootResolver (..),
                                                         ResolverQ,
                                                         Undefined (..))

import           Data.Map.Monoidal                      (MonoidalMap)
import qualified Data.Map.Monoidal                      as MMap
import           Reflex.Patch                           (Additive,
                                                         Group (negateG))
import qualified Reflex.Query.Class                     as Q

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

instance Hashable GetDeityArgs

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

app :: Application
app request respond = do

  -- (state, html) <- renderStatic' . runHydratableT . runSourceT ("http://graphql.localhost:3000", constant Map.empty) $
  (state, html) <- renderStatic' . runHydratableT $ runSourceT ("http://graphql.localhost:3000", constant Map.empty) $
    el "html" $ do
      el "head" $ do
        elAttr "script" ("src" =: "http://jsaddle.localhost:3000/jsaddle.js") blank
        elAttr "link" ("rel" =: "stylesheet" <> "href" =: ("http://static.localhost:3000/" <> test_css)) blank
      el "body" $ do
        text "hello"
        buildE <- getPostBuild
        clickE <- button "click"
        textD <- holdDyn "before click" $ "afterClick " <$ buildE
        dynText textD
        -- queryW
        runViewT (constLocHandler $ T.decodeUtf8 . rawPathInfo $ request) appW

  let status = case state of
        Right _ -> ok200
        Left _  -> status404

  respond $ responseLBS status [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict html

-- type MyQuery a = MonoidalMap Int a

newtype MyQuery a = MyQuery (MonoidalMap Int a)
  deriving (Eq, Monoid, Semigroup, Group, Additive)


-- instance Functor MyQuery where
  -- fmap (MyQuery m) = fmap m

instance (Monoid a, Eq a) => Q.Query (MyQuery a) where
  type QueryResult (MyQuery a) = MonoidalMap Int a
  crop (MyQuery (m :: _)) (res :: _) = res -- MMap.filter (\(k :: _) -> MMap.member k m) res

-- instance (Group a) => Group (MyQuery a) where
--   negateG = fmap negateG

-- instance (Semigroup a) => Semigroup (MyQuery a) where
--   (<>) (MyQuery a) (MyQuery b) = MyQuery $ a <> b

-- instance (Monoid a) => Monoid (MyQuery a) where
--   mempty = MyQuery MMap.empty
--   mappend = (<>)

-- instance (Semigroup a) => Additive (MyQuery a)

instance Num a => Group (Sum a) where
  negateG (Sum i) = Sum $ negate i

-- data QueryGQL a = QueryGQL (MonoidalMap Int a)
--   deriving (Eq, Functor)

-- ryantrinkle> aveltras: yes, or in your case it could be the cache
-- <ryantrinkle> it doesn't *have* to be constantly-updating to work
-- <ryantrinkle> (but you might need to put in something to ensure values aren't "too stale")
-- <aveltras> Map GraphqlQuery Int would be the "a" of Query a ?
-- <ryantrinkle> yeah
-- <ryantrinkle> er
-- <ryantrinkle> yes
-- <ryantrinkle> and QueryResult (Map GraphqlQuery Int) would be Map GraphqlQuery GraphqlResponse

-- data IsGraphqlQuery


type XhrConstraints t m = (HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m, HasJSContext m, MonadIO m, MonadJSM m)

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


queryHandlerXhr :: (XhrConstraints t m, PostBuild t m, Monad m, Reflex t, MonadHold t m) => Dynamic t (MyQuery (Sum Int)) -> m (Dynamic t (QueryResult (MyQuery (Sum Int))))
queryHandlerXhr queryD = do
  buildE <- getPostBuild
  let queryE = updated queryD
  respE :: Event t (MonoidalMap Int XhrResponse) <- performRequestsAsync ((\(MyQuery m) -> (postJson "http://api.localhost:3000" . getSum) <$> m) <$> (leftmost [queryE, tagPromptlyDyn queryD buildE]))
  holdDyn MMap.empty $ (filterMaybes <$> ((fmap . fmap) (fmap Sum . decodeXhrResponse) respE))

  where

    filterMaybes :: (Semigroup a, Ord k) => MonoidalMap k (Maybe a) -> MonoidalMap k a
    filterMaybes = MMap.foldMapWithKey f

    f :: (Semigroup a, Ord k) => k -> Maybe a -> MonoidalMap k a
    f k (Just a) = MMap.singleton k a
    f k Nothing  = mempty

queryDynUniq :: ( Monad m
                , Reflex t
                , MonadQuery t q m
                , MonadHold t m
                , MonadFix m
                , Eq (QueryResult q)
                )
             => Dynamic t q
             -> m (Dynamic t (QueryResult q))
queryDynUniq = holdUniqDyn <=< queryDyn


queryW :: (XhrConstraints t m, Reflex t, MonadHold t m, DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m) => m ()
queryW = do
  rec
    v <- queryHandlerXhr nubbedVs
    (_a, vs) <- runQueryT widgetWithQuery v
    nubbedVs <- holdUniqDyn $ incrementalToDynamic vs
  blank

widgetWithQuery :: (XhrConstraints t m, MonadFix m, MonadHold t m, MonadQuery t (MyQuery (Sum Int)) m, PostBuild t m, DomBuilder t m) => m ()
widgetWithQuery = do
  clickE <- button "click"
  countD <- count clickE
  resultD <- queryDynUniq $ ffor countD $ \str -> MyQuery $ MMap.singleton (5 :: Int) (Sum str)
  let textD = ffor (traceDyn "debug" resultD) (\m -> maybe "nothing" (T.pack . show . getSum) $ MMap.lookup (5 :: Int) m)
  dynText textD
  blank

mainJS :: JSM ()
mainJS = Main.mainWidget $ do
  buildE <- getPostBuild
  clickE <- button "click"
  textD <- holdDyn "before click" $ "afterClick " <$ leftmost [buildE, clickE]
  dynText textD
  queryW
  _ <- runSourceT ("http://graphql.localhost:3000", constant Map.empty) $ runViewT browserLocHandler appW
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

appW :: (DomBuilder t m, MonadHold t m, HasSource t js m, HasView t View ViewError m, PerformEvent t m, Prerender js t m, PostBuild t m) => m ()
appW = do
  viewD <- askView
  void $ dyn $ (\case
                   Right v -> case v of
                     HomeV    -> do
                       text "home"
                       linkTo ContactV $ text "go contact"
                       graphQLwidget
                     ContactV -> do
                       text "contact"
                       linkTo HomeV $ text "go home"
                   Left e -> case e of
                     ViewError -> do
                       text "not found"
                       linkTo HomeV $ text "go home"
                       linkTo ContactV $ text "go contact"
               ) <$> viewD

graphQLwidget :: (HasSource t js m, PostBuild t m, MonadHold t m, DomBuilder t m) => m ()
graphQLwidget = do
  buildE <- getPostBuild
  clickE <- button "click"
  responseE :: Event t (Either String GetDeity) <- fetchData (GetDeityArgs "tac" <$ leftmost [buildE, clickE])
  -- responseE :: Event t (Either String GetDeity) <- xhrQuery
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
  print "query"
  bs <- strictRequestBody request
  resp <- api bs
  respond $ responseLBS ok200 [(hContentType, "application/json")] resp
