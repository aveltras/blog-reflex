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
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Coerce            (coerce)
import           Data.Hashable
import           Data.IORef
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Morpheus.Client
import           Data.Morpheus.Types.IO
import           Data.Proxy             (Proxy (..))
import qualified Data.Text.Encoding     as T
import           GHCJS.DOM.Types        (MonadJSM)
import           Network.HTTP.Req
import           Reflex.Dom.Core        hiding (Query, Value)


type HasSource t request m = (Requester t (Client m), Requester t m, Request (Client m) ~ request, Request m ~ request, Response (Client m) ~ Either String, Response m ~ Either String)

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
  , Show wireFormat
  ) => (Event t (Map Int wireFormat) -> m (Event t (Map Int wireFormat)))
  -> (forall b. request b -> (wireFormat, wireFormat -> response b))
  -> SourceT t request response m a
  -> m a
runSourceT requestHandler codec (SourceT widget) = mdo
  wireResponsesE <- requestHandler wireRequestsE
  (result, requestE) <- runRequesterT widget responseE
  -- (wireRequestsE, responseE) <- matchResponsesWithRequests codec requestE $ fmapMaybe id (safeHead . Map.toList <$> traceEvent "raw response" wireResponsesE)
  (wireRequestsE, responseE) <- matchResponsesWithRequests codec requestE $ traceEvent "runSourceT - raw response" (head . Map.toList <$> wireResponsesE)
  pure result

safeHead :: [a] -> Maybe a
safeHead = \case
  [] -> Nothing
  x:_ -> Just x

data IsGraphQLQuery query = (FromJSON query, Fetch query, Show query) => IsGraphQLQuery { unGraphQLQuery :: Args query }

type WireFormat = BS.ByteString

graphqlCodec :: forall request. IsGraphQLQuery request -> (WireFormat, WireFormat -> Either String request)
graphqlCodec (IsGraphQLQuery args) = (toWire, fromWire)
  where
    toWire = BL.toStrict $ encode $ buildReq (Proxy :: Proxy request) args
    fromWire wire = eitherDecodeStrict wire >>= \case
      JSONResponse { responseData = Just x } -> Right x
      invalidResponse -> Left $ show invalidResponse

reflexXhrHandler :: (XhrConstraints t m) => XhrRequestConfig () -> Behavior t (Map Int WireFormat) -> Event t (Map Int WireFormat) -> m (Event t (Map Int WireFormat))
reflexXhrHandler xhrConfig cacheB requestsE = do
  let partitionedRequestsE = partitionRequests <$> attach cacheB requestsE
      cachedResponsesE = ffor partitionedRequestsE fst
      xhrRequestsE = ffor partitionedRequestsE snd
  xhrResponsesE <- performRequestsAsync xhrRequestsE
  pure $ traceEvent "frontend - xhr result" $ ffilter ((/=) Map.empty) $ cachedResponsesE <> (fmap . fmap) extractBody xhrResponsesE

    where

      partitionRequests :: (Map Int WireFormat, Map Int WireFormat) -> (Map Int WireFormat, Map Int (XhrRequest WireFormat))
      partitionRequests (cache, requests) = Map.foldrWithKey f (Map.empty, Map.empty) requests
        where
          f k a (x1, x2) = case Map.lookup (hash a) cache of
            Nothing -> (x1, Map.insert k (toXhrRequest a) x2)
            Just bs -> (Map.insert k bs x1, x2)

      toXhrRequest :: WireFormat -> XhrRequest WireFormat
      toXhrRequest wire = xhrRequest "POST" "http://graphql.localhost:3000" $ xhrConfig & xhrRequestConfig_sendData .~ wire

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

reqXhrHandler :: (PerformEvent t m, MonadIO (Performable m)) => IORef (Map Int WireFormat) -> Event t (Map Int WireFormat) -> m (Event t (Map Int WireFormat))
reqXhrHandler cache requestsE = traceEvent "server - xhr result" <$> (performEvent $ fmap toXhrRequest requestsE)
  where
    toXhrRequest = traverse $ \wire -> runReq defaultHttpConfig $ do
      response <- responseBody <$> req POST -- method
                                   (http "graphql.localhost") -- safe by construction URL
                                   (ReqBodyBs wire) -- use built-in options or add your own
                                   bsResponse -- specify how to interpret response
                                   (port 3000) -- query params, headers, explicit port number, etc.
      liftIO $ modifyIORef' cache $ Map.insert (hash wire) response
      pure response


































