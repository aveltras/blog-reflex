module Main where

import           Control.Monad                          (forM_, void)
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
import           Data.Serialize                         (Serialize)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
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
                                                         Resolver, ResolverQ,
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
import qualified Network.HTTP.Req                       as Req
import           Sessionula.Backend.File

mkStaticApp "static"

importGQLDocumentWithNamespace "schema.graphql"

defineByDocumentFile "schema.graphql" [gql|
  query GetDeity ($goName: String!) {
    deity (name: $goName) {
      name
      power
    }
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

bodyWidget :: (DynamicWriter t Text m, MonadIO (Performable m), Prerender js t m, PostBuild t m, HasSource t IsGraphQLQuery m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilder t m, HasView t View ViewError m) => m ()
bodyWidget = appW

data View = Homepage
          | Contact

instance PathPiece View where
  fromPathPiece = \case
    "" -> Just Homepage
    "contact" -> Just Contact
    _ -> Nothing
  toPathPiece = \case
    Homepage -> ""
    Contact -> "contact"

headWidget :: (DomBuilder t m, PostBuild t m, Prerender js t m) => Dynamic t Text -> m ()
headWidget headD = do
  elAttr "script" ("src" =: "http://jsaddle.blog.local:3000/jsaddle.js") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: ("http://static.blog.local:3000/" <> main_css)) blank
  prerender_ (el "title" $ dynText headD) (el "title" $ dynText headD)

appW :: (DynamicWriter t Text m, MonadIO (Performable m), DomBuilder t m, TriggerEvent t m, HasSource t IsGraphQLQuery m, MonadHold t m, HasView t View ViewError m, PerformEvent t m, Prerender js t m, PostBuild t m) => m ()
appW = do
  viewD <- askView
  void $ dyn $ (\case
                   Right v -> case v of
                     Homepage    -> do
                       tellDyn $ constDyn "home"
                       text "home"
                       linkTo Contact $ text "go contact"
                       graphQLwidget
                     Contact -> do
                       tellDyn $ constDyn "contact"
                       linkTo Homepage $ text "go home"
                   Left e -> case e of
                     ViewError -> do
                       linkTo Homepage $ text "go home"
                       linkTo Contact $ text "go contact"
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

  eval ("console.log('"<>show cacheMap'<>"')" :: String)

  runHydrationWidgetWithHeadAndBody (pure ()) $ \appendHead appendBody -> mdo
    void $ appendHead (headWidget headD)
    ((_, headD), _) <- appendBody $ runSourceT cacheMap' (reflexXhrHandler (def & xhrRequestConfig_withCredentials .~ True)) graphqlCodec $ runDynamicWriterT $ runViewT browserLocHandler $ bodyWidget
    blank

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
renderStatic' :: StaticWidget x (Dynamic DomTimeline a, Behavior DomTimeline b) -> IO ((a, b), BS.ByteString)
renderStatic' w = do
  runDomHost $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    nextRunWithReplaceKey <- newRef 0
    let env0 = StaticDomBuilderEnv True Nothing nextRunWithReplaceKey
    ((res, bs), FireCommand fire) <- hostPerformEventT $ runStaticDomBuilderT (runPostBuildT w postBuild) env0
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    bs' <- sample bs
    a <- sample . current $ fst res
    b <- sample $ snd res
    return ((a, b), BL.toStrict $ B.toLazyByteString bs')


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
    , deityPower = pure (Just "Shapeshifting") -- liftEither $ pure $ Left "fail example" --
    }

loginResolver :: MutationLoginArgs -> Resolver MUTATION () (RIO Ctx) (Maybe (User (Resolver MUTATION () (RIO Ctx))))
loginResolver MutationLoginArgs {..} = do
  pure $ Just $ User { userEmail = pure "email" }

logoutResolver :: Resolver MUTATION () (RIO Ctx) Bool
logoutResolver = error "not implemented"

sendMessageResolver :: MutationSendMessageArgs -> Resolver MUTATION () (RIO Ctx) Bool
sendMessageResolver = error "not implemented"
