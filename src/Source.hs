{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Source where

import           Control.Lens
import           Control.Monad                  (forM, forM_, void, (>=>))
import           Control.Monad.Fix              (MonadFix)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Reader.Class     (MonadReader, ask)
import           Control.Monad.Ref
import           Control.Monad.Trans.Class      (MonadTrans, lift)
import           Control.Monad.Trans.Reader     (Reader, ReaderT, runReaderT)
import           Data.Aeson
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Coerce                    (coerce)
import           Data.Constraint.Forall
import           Data.Hashable
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Morpheus.Client
import           Data.Morpheus.Client
import           Data.Morpheus.Document
import           Data.Morpheus.Types.IO
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           GHC.Generics
import           GHC.IORef
import           "ghcjs-dom" GHCJS.DOM.Document (Document)
import           GHCJS.DOM.Types                (MonadJSM)
import           Network.HTTP.Req
import           Reflex.Dom.Core                hiding (Query)
import           Reflex.Host.Class
import           System.Random
import           Type.Reflection
import           UnliftIO.MVar


-- class (Monad m) => HasSource t m | m -> t where
--   query :: Event t (Request m a) -> m (Event t (Response m a))

-- class (Monad m, Reflex t) => HasSource t js m | m -> t js where
--   fetchData :: forall query. (Typeable query, Fetch query, Hashable (Args query), FromJSON query) => Event t (Args query) -> m (Event t (Either String query))
--   default fetchData :: forall query m' tx. (Typeable query, Fetch query, Hashable (Args query), HasSource t js m', m ~ tx m', MonadTrans tx, FromJSON query) => Event t (Args query) -> m (Event t (Either String query))
--   fetchData = lift . fetchData

type HasSource t request m = (Requester t m, Request m ~ request, Response m ~ Either String)

newtype SourceT t request response m a
  = SourceT { unSourceT :: RequesterT t request response m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , DomBuilder t
    , NotReady t
    , MonadFix
    , MonadIO
    , MonadJSM
    , MonadHold t
    , Requester t
    , MonadSample t
    , Prerender js t
    , PostBuild t
    , PerformEvent t
    , TriggerEvent t
    )

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (SourceT t request response m) where
  runWithReplace a e = SourceT $ runWithReplace (coerce a) (coerce <$> e)
  traverseDMapWithKeyWithAdjust f m e = SourceT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = SourceT $ traverseIntMapWithKeyWithAdjust (\k v -> coerce $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = SourceT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) m e

runSourceT :: forall t request response wireFormat m a.
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) => (Event t (Map Int wireFormat) -> m (Event t (Map Int wireFormat)))
  -> (forall b. request b -> (wireFormat, wireFormat -> response b))
  -> SourceT t request response m a
  -> m a
runSourceT requestHandler codec (SourceT widget) = mdo
  wireResponsesE <- requestHandler wireRequestsE
  (result, requestE) <- runRequesterT widget responseE
  (wireRequestsE, responseE) <- matchResponsesWithRequests codec requestE $ head . Map.toList <$> wireResponsesE
  pure result

data IsGraphQLQuery query = (FromJSON query, Fetch query, Show query) => IsGraphQLQuery { unGraphQLQuery :: Args query }

type WireFormat = BS.ByteString

graphqlCodec :: forall request. IsGraphQLQuery request -> (WireFormat, WireFormat -> Either String request)
graphqlCodec (IsGraphQLQuery args) = (toWire, fromWire)
  where
    toWire = BL.toStrict $ encode $ buildReq (Proxy :: Proxy request) args
    fromWire wire = eitherDecodeStrict wire >>= \case
      JSONResponse { responseData = Just x } -> Right x
      invalidResponse -> Left $ show invalidResponse

reflexXhrHandler :: (XhrConstraints t m) => XhrRequestConfig () -> Event t (Map Int WireFormat) -> m (Event t (Map Int WireFormat))
reflexXhrHandler xhrConfig requestsE = do
  xhrResponsesE <- performRequestsAsync $ (fmap . fmap) toXhrRequest requestsE
  pure $ (fmap . fmap) extractBody xhrResponsesE

    where

      toXhrRequest :: BS.ByteString -> XhrRequest WireFormat
      toXhrRequest wire = xhrRequest "POST" "tacotac" $ xhrConfig & xhrRequestConfig_sendData .~ wire

      extractBody :: XhrResponse -> WireFormat
      extractBody xhrResponse = case xhrResponse ^. xhrResponse_responseText of
        Nothing  -> error "boom"
        Just txt -> T.encodeUtf8 $ txt


type XhrConstraints t m =
  ( HasJSContext (Performable m)
  , MonadJSM (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , HasJSContext m
  , MonadIO m
  , MonadJSM m
  )

-- instance (MonadIO m, MonadIO (HostFrame t), ReflexHost t, Reflex t, Ref m ~ IORef) => HasSource t js (

reqXhrHandler :: (PerformEvent t m, MonadIO (Performable m)) => Event t (Map Int WireFormat) -> m (Event t (Map Int WireFormat))
reqXhrHandler = performEvent . fmap toXhrRequest
  where
    toXhrRequest = traverse $ \wire -> runReq defaultHttpConfig $ do
      responseBody <$> req POST -- method
                           (http "graphql.localhost") -- safe by construction URL
                           (ReqBodyBs wire) -- use built-in options or add your own
                           bsResponse -- specify how to interpret response
                           (port 3000) -- query params, headers, explicit port number, etc.













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


test :: (XhrConstraints t m, PerformEvent t m, DomBuilder t m, Monad m, MonadHold t m, MonadFix m, PostBuild t m, Reflex t) => m ()
test = do
  res <- runSourceT (reflexXhrHandler def) graphqlCodec $ do
    buildE <- getPostBuild
    responseE :: Event t (Either String GetDeity) <- requesting $ (IsGraphQLQuery (GetDeityArgs "tac")) <$ buildE
    undefined
  blank



-- instance HasSource t js m => HasSource t js (ReaderT r m)
-- instance HasSource t js m => HasSource t js (EventWriterT t w m)
-- instance HasSource t js m => HasSource t js (StaticDomBuilderT t m)
-- instance HasSource t js m => HasSource t js (HydrationDomBuilderT t js m)
-- instance HasSource t js m => HasSource t js (PostBuildT t m)
-- instance HasSource t js m => HasSource t js (HydratableT m)
-- instance (HasSource t js m, ReflexHost t, MonadTrans (PerformEventT t)) => HasSource t js (PerformEventT t m)




  -- let xhrRequests = (fmap . fmap) (\req -> XhrRequest @ByteString "POST" endpoint (xhrConfig & xhrRequestConfig_sendData .~ req)) rawRequestMap
  -- xhrResponses <- traceEventWith (const "respxhr") <$> (performRequestsAsyncWithError xhrRequests)
  -- (result, onRequest) <- runRequesterT widget onResponse
  -- (rawRequestMap, onResponse) <- matchResponsesWithRequests codec onRequest (head . M.toList <$> xhrResponses)
  -- return result



-- -- instance (Monad m, Reflex t, HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m) => HasSource t GhcjsDomSpace (SourceT t GhcjsDomSpace m) where
-- instance (Monad m, Reflex t, HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m) =>
--   HasSource t js (SourceT t js (HydrationDomBuilderT GhcjsDomSpace t m)) where

--   fetchData queryE = do
--     performEvent $ fetch xhrFetch <$> queryE

--     where

--       xhrFetch queryBS = do
--         (endpoint, _) <- ask

--         let req = xhrRequest "POST" endpoint $ def & xhrRequestConfig_sendData .~ BL.toStrict queryBS

--         resultVar <- newEmptyMVar
--         void $ newXMLHttpRequest req $ liftIO . putMVar resultVar
--         resp <- takeMVar resultVar

--         let body = case resp ^. xhrResponse_responseText of
--               Nothing  -> error "boom"
--               Just txt -> BL.fromStrict . T.encodeUtf8 $ txt

--         pure body





  -- -> IsCodec request response wireFormat
-- data IsCodec request response wireFormat = IsCodec (forall b. request b -> (wireFormat, wireFormat -> response b))

-- requesting :: Event t (Request m a) -> m (Event t (Response m a))


-- class (Monad m, Reflex t) => HasSource t js m | m -> t js where
--   fetchData :: forall query. (Typeable query, Fetch query, Hashable (Args query), FromJSON query) => Event t (Args query) -> m (Event t (Either String query))
--   default fetchData :: forall query m' tx. (Typeable query, Fetch query, Hashable (Args query), HasSource t js m', m ~ tx m', MonadTrans tx, FromJSON query) => Event t (Args query) -> m (Event t (Either String query))
--   fetchData = lift . fetchData


-- instance HasSource t js m => HasSource t js (ReaderT r m)
-- instance HasSource t js m => HasSource t js (EventWriterT t w m)
-- instance HasSource t js m => HasSource t js (StaticDomBuilderT t m)
-- instance HasSource t js m => HasSource t js (HydrationDomBuilderT t js m)
-- instance HasSource t js m => HasSource t js (PostBuildT t m)
-- instance HasSource t js m => HasSource t js (HydratableT m)
-- instance (HasSource t js m, ReflexHost t, MonadTrans (PerformEventT t)) => HasSource t js (PerformEventT t m)

-- type SourceEnv t = (Text, Behavior t (Map (Int, SomeTypeRep) SomeTypeRep))

-- newtype SourceT t s m a
--   = SourceT { unSourceT :: ReaderT (SourceEnv t) m a}
--   deriving
--     ( Functor
--     , Applicative
--     , Monad
--     , DomBuilder t
--     , NotReady t
--     , MonadIO
--     , MonadReader (SourceEnv t)
--     , MonadHold t
--     , MonadJSM
--     , MonadSample t
--     , MonadQuery t q
--     , MonadFix
--     , MonadTrans
--     , PostBuild t
--     , PerformEvent t
--     , Prerender js t
--     , TriggerEvent t
--     )

-- -- type SourceEnv t = (Text, Behavior t (Map (Int, SomeTypeRep) Bool))

-- instance (MonadIO m, MonadIO (HostFrame t), ReflexHost t, Reflex t, Ref m ~ IORef) => HasSource t js (
--   SourceT t js (HydratableT (PostBuildT t (StaticDomBuilderT t (PerformEventT t m)) ))
--   ) where

--   fetchData :: forall query. (Typeable query, Fetch query, Hashable (Args query), FromJSON query) => Event t (Args query) -> SourceT t js (HydratableT (PostBuildT t (StaticDomBuilderT t (PerformEventT t m)) )) (Event t (Either String query))
--   fetchData queryE = do

--     (_endpoint, cache) <- ask

--     lookupCache :: Map (Int, SomeTypeRep) SomeTypeRep <- sample cache

--     performEvent $ do
--       coincidence $ ffor queryE $ \(args) -> do
--         case Map.lookup (hash args, someTypeRep (Proxy :: Proxy query)) lookupCache of
--           Just (rep) -> case typeRep @query `eqTypeRep` (typeOf rep) of
--             Just HRefl -> (pure $ Right rep) <$ queryE
--             Nothing    -> fetch xhrFetch <$> queryE
--           Nothing         -> fetch xhrFetch <$> queryE

--     where

--       xhrFetch queryBS = do

--         runReq defaultHttpConfig $ do
--           r <-
--             req
--             POST -- method
--             (http "graphql.localhost") -- safe by construction URL
--             (ReqBodyLbs queryBS) -- use built-in options or add your own
--             lbsResponse -- specify how to interpret response
--             (port 3000) -- query params, headers, explicit port number, etc.
--           pure $ responseBody r

-- instance (Monad m, Reflex t) => HasSource t js (SourceT t js (HydrationDomBuilderT js t m)) where
--   fetchData queryE = pure never

-- -- instance (Monad m, Reflex t, HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m) => HasSource t GhcjsDomSpace (SourceT t GhcjsDomSpace m) where
-- instance (Monad m, Reflex t, HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m) =>
--   HasSource t js (SourceT t js (HydrationDomBuilderT GhcjsDomSpace t m)) where

--   fetchData queryE = do
--     performEvent $ fetch xhrFetch <$> queryE

--     where

--       xhrFetch queryBS = do
--         (endpoint, _) <- ask

--         let req = xhrRequest "POST" endpoint $ def & xhrRequestConfig_sendData .~ BL.toStrict queryBS

--         resultVar <- newEmptyMVar
--         void $ newXMLHttpRequest req $ liftIO . putMVar resultVar
--         resp <- takeMVar resultVar

--         let body = case resp ^. xhrResponse_responseText of
--               Nothing  -> error "boom"
--               Just txt -> BL.fromStrict . T.encodeUtf8 $ txt

--         pure body

-- runSourceT :: SourceEnv t -> SourceT t js m a -> m a
-- runSourceT cs (SourceT m) = runReaderT m cs
