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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Control.Monad                          (forM_, void)
import           Control.Monad.IO.Class                 (MonadIO)
import           Control.Monad.Ref
import           Data.Aeson
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Builder                as B
import qualified Data.ByteString.Char8                  as SC8
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.ByteString.Lazy.Char8             as C8
import           Data.Dependent.Sum                     (DSum (..))
import           Data.Functor.Identity                  (Identity (..))
import           Data.Map
import qualified Data.Map                               as Map
import           Data.Serialize                         (Serialize)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import           RIO.Time
-- import           GHC.IORef
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
import           Reflex.Host.Class
import           RIO
import           Web.PathPieces

import           Data.Morpheus                          (interpreter)
import           Data.Morpheus.Client
import           Data.Morpheus.Document
import           Data.Morpheus.Types                    (GQLRootResolver (..),
                                                         MUTATION, QUERY,
                                                         Resolver, ResolverM,
                                                         ResolverQ,
                                                         Undefined (..))


import           Source
import           View

import qualified GHCJS.DOM                              as DOM
import qualified GHCJS.DOM.Document                     as DOM
import qualified GHCJS.DOM.HTMLScriptElement            as DOM
import qualified GHCJS.DOM.ParentNode                   as DOM
import qualified GHCJS.DOM.Types                        as DOM

import qualified Sessionula                             as Session (Handle, defaultConfig,
                                                                    setup)
import qualified Sessionula.Extra                       as Session
import qualified Sessionula.Frontend.Wai                as Session

import qualified Data.CaseInsensitive                   as CI
import           Network.HTTP.Client                    (Cookie (..),
                                                         createCookieJar,
                                                         parseRequest)
import qualified Network.HTTP.Req                       as Req
import           Sessionula.Backend.File
import           Web.Cookie

import           App.Database.Schema

mkStaticApp "static"

importGQLDocumentWithNamespace "schema.graphql"

defineByDocumentFile
  "schema.graphql"
  [gql|
    query GetDeity ($goName: String!)
    {
      deity (name: $goName)
      { name, power }
    }
  |]

main :: IO ()
main = do
  let port = 3000

  manager <- Session.setup Session.defaultConfig =<< fileStorage "/tmp/sessions"


  jsApp <- JW.jsaddleOr defaultConnectionOptions (mainJS >> syncPoint) $ JW.jsaddleAppWithJs $ JW.jsaddleJs' (Just $ "http://jsaddle.blog.local:" <> (C8.pack . show) port) False
  Warp.run port $ vhost
    [ ((==) (Just . T.encodeUtf8 . T.pack . (<>) "static.blog.local:" $ show port) . requestHeaderHost, gzip def $ staticApp True)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "jsaddle.blog.local:" $ show port) . requestHeaderHost, cors (const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] } )) $ jsApp)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "graphql.blog.local:" $ show port) . requestHeaderHost, laxCors $ sessionMiddleware manager $ graphqlApp)
    , ((==) (Just . T.encodeUtf8 . T.pack . (<>) "api.blog.local:" $ show port) . requestHeaderHost, laxCors echoApp)
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

echoApp :: Application
echoApp request respond = do
  lbs <- lazyRequestBody request
  respond $ responseLBS ok200 [(hContentType, "application/json")] lbs

placeholder = "%%%"

app :: Application
app request respond = do

  -- let host = fromMaybe "" $ requestHeaderHost request
  -- clientRequest <- parseRequest $ SC8.unpack $ "http://" <> host <> rawPathInfo request
  -- let cookies :: _ = maybe [] id $ parseCookies <$> Prelude.lookup hCookie (requestHeaders request)

  let headers = Prelude.filter ((==) (CI.mk "Cookie") . fst) $ requestHeaders request
      clientOptions = flip foldMap headers $ \(n, v) -> Req.header (CI.original n) v
      clientOptions' = Req.port 3000 <> clientOptions
  -- now <- getCurrentTime
  -- let future = addUTCTime 10 now

  -- let clientCookies :: [Cookie] =
  --       flip fmap cookies $ \(n,v) -> Cookie
  --         { cookie_name = n
  --         , cookie_value = v
  --         , cookie_expiry_time = future
  --         , cookie_domain = ".blog.local"
  --         , cookie_path = "/"
  --         , cookie_creation_time = now
  --         , cookie_last_access_time = now
  --         , cookie_persistent = False
  --         , cookie_host_only = False
  --         , cookie_secure_only = False
  --         , cookie_http_only = True
  --         }

  cacheRef <- newIORef Map.empty

  let commentedPlaceholder = "<!--" <> placeholder <> "-->"

  (state, html) <- renderStatic' . runHydratableT . runSourceT (reqXhrHandler clientOptions' cacheRef) graphqlCodec . runViewT (constLocHandler $ T.decodeUtf8 . rawPathInfo $ request) $
    el "html" $ do
      el "head" $ void headWidget >> (comment $ T.decodeUtf8 placeholder)
      el "body" bodyWidget

  let status = case state of
        Right _ -> ok200
        Left _  -> status404

  cache <- readIORef cacheRef

  let (a, b) = BS.breakSubstring commentedPlaceholder html
      prerenderedHtml = a <> "<script data-prerenderblob>//" <> (BL.toStrict . encode $ T.decodeUtf8 <$> cache) <> "</script>" <> BS.drop (BS.length commentedPlaceholder) b

  respond $ responseLBS status [(hContentType, "text/html")] $ "<!doctype html>" <> BL.fromStrict prerenderedHtml

-- translateCookie :: SetCookie -> Cookie
-- translateCookie c = Cookie
--   { cookie_name = setCookieName c
--   , cookie_value = setCookieValue c
--   , cookie_expiry_time = undefined
--   , cookie_domain = undefined
--   , cookie_path = maybe "" id $ setCookiePath c
--   , cookie_creation_time = undefined
--   , cookie_last_access_time = undefined
--   , cookie_persistent = undefined
--   , cookie_host_only = undefined
--   , cookie_secure_only = setCookieSecure c
--   , cookie_http_only = setCookieHttpOnly c
--   }


-- data SetCookie = SetCookie
--     { setCookieName :: S.ByteString -- ^ The name of the cookie. Default value: @"name"@
--     , setCookieValue :: S.ByteString -- ^ The value of the cookie. Default value: @"value"@
--     , setCookiePath :: Maybe S.ByteString -- ^ The URL path for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the path of the request that sets the cookie).
--     , setCookieExpires :: Maybe UTCTime -- ^ The time at which to expire the cookie. Default value: @Nothing@ (The browser will default to expiring a cookie when the browser is closed).
--     , setCookieMaxAge :: Maybe DiffTime -- ^ The maximum time to keep the cookie, in seconds. Default value: @Nothing@ (The browser defaults to expiring a cookie when the browser is closed).
--     , setCookieDomain :: Maybe S.ByteString -- ^ The domain for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the current domain).
--     , setCookieHttpOnly :: Bool -- ^ Marks the cookie as "HTTP only", i.e. not accessible from Javascript. Default value: @False@
--     , setCookieSecure :: Bool -- ^ Instructs the browser to only send the cookie over HTTPS. Default value: @False@
--     , setCookieSameSite :: Maybe SameSiteOption -- ^ The "same site" policy of the cookie, i.e. whether it should be sent with cross-site requests. Default value: @Nothing@
--     }

-- data Cookie = Cookie
--   { cookie_name :: S.ByteString
--   , cookie_value :: S.ByteString
--   , cookie_expiry_time :: UTCTime
--   , cookie_domain :: S.ByteString
--   , cookie_path :: S.ByteString
--   , cookie_creation_time :: UTCTime
--   , cookie_last_access_time :: UTCTime
--   , cookie_persistent :: Bool
--   , cookie_host_only :: Bool
--   , cookie_secure_only :: Bool
--   , cookie_http_only :: Bool
--   }

headWidget :: (DomBuilder t m) => m ()
headWidget = do
  elAttr "script" ("src" =: "http://jsaddle.blog.local:3000/jsaddle.js") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: ("http://static.blog.local:3000/" <> main_css)) blank

bodyWidget :: (MonadIO (Performable m), Prerender js t m, PostBuild t m, HasSource t IsGraphQLQuery m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilder t m, HasView t View ViewError m) => m ()
bodyWidget = appW

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

      -- cacheMap' = maybe Map.empty (fmap T.encodeUtf8) cacheMap -- doesn't work
      cacheMap' = Map.empty -- works

  -- eval ("console.log('"<> show t <>"')" :: String)
  -- eval ("console.log('"<> show cacheMap' <>"')" :: String)



  let
    bodyWidget' = do
      _ <- runSourceT (reflexXhrHandler (def & xhrRequestConfig_withCredentials .~ True) (constant cacheMap')) graphqlCodec $ runViewT browserLocHandler appW
      blank

    -- w :: (FrontendWidget () -> TriggerEventT DomTimeline (DomCoreWidget ()) x)
      -- -> (FrontendWidget () -> TriggerEventT DomTimeline (DomCoreWidget ()) x)
      -- -> TriggerEventT DomTimeline (DomCoreWidget ()) x
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
  responseE :: Event t (Either String GetDeity) <- requesting $ (IsGraphQLQuery (GetDeityArgs "tac")) <$ buildE
  widgetD <- holdDyn "initial" $ ffor responseE $ \r -> case r of
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


graphqlApp :: Application
graphqlApp request respond = do
  let sessionHandle = Session.extractSession request
  bs <- lazyRequestBody request
  resp <- runRIO (Ctx sessionHandle) $ interpreter rootResolver bs
  respond $ responseLBS ok200 [ (hContentType, "application/json") ] resp

newtype UserId = UserId Int32
  deriving (Serialize)

data Ctx = Ctx
  { ctxSession :: Session.Handle UserId
  }

instance Session.HasSession Ctx where
  type Auth Ctx = UserId
  sessionL = lens ctxSession (\x y -> x { ctxSession = y })

rootResolver :: GQLRootResolver (RIO Ctx) () Query Mutation Undefined
rootResolver = GQLRootResolver
  { queryResolver = Query
    { queryDeity = deityResolver
    , queryArticles = articlesResolver
    , queryArticle = articleResolver
    , queryPage = pageResolver
    }
  , mutationResolver = Mutation
    { mutationLogin = loginResolver
    , mutationLogout = logoutResolver
    , mutationSendMessage = sendMessageResolver
    }
  , subscriptionResolver = Undefined
  }

pageResolver :: QueryPageArgs -> (Resolver QUERY () (RIO Ctx) (Maybe (Page (Resolver QUERY () (RIO Ctx)))))
pageResolver = error "not implemented"

articleResolver :: QueryArticleArgs -> (Resolver QUERY () (RIO Ctx) (Maybe (Article (Resolver QUERY () (RIO Ctx)))))
articleResolver = error "not implemented"

articlesResolver :: Resolver QUERY () (RIO Ctx) (Maybe [Maybe (Article (Resolver QUERY () (RIO Ctx)))])
articlesResolver = error "not implemented"

deityResolver :: QueryDeityArgs -> ResolverQ () (RIO Ctx) Deity
deityResolver QueryDeityArgs {..} = do
  mAuth <- lift $ Session.authStatus
  lift $ case mAuth of
    Nothing -> Session.authenticate $ UserId 1
    Just _  -> Session.logout
  pure Deity
    { deityName = pure $ if isJust mAuth then "Authenticated Morpheus" else "Guest Morpheus"
    , deityPower = pure (Just "Shapeshifting")
    }

loginResolver :: MutationLoginArgs -> Resolver MUTATION () (RIO Ctx) (Maybe (User (Resolver MUTATION () (RIO Ctx))))
loginResolver MutationLoginArgs {..} = do
  pure $ Just $ User { userEmail = pure "email" }

logoutResolver :: Resolver MUTATION () (RIO Ctx) Bool
logoutResolver = error "not implemented"

sendMessageResolver :: MutationSendMessageArgs -> Resolver MUTATION () (RIO Ctx) Bool
sendMessageResolver = error "not implemented"
