{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Source where

import           Control.Lens
import           Control.Monad                  (void)
import           Control.Monad.Fix              (MonadFix)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Reader.Class     (MonadReader, ask)
import           Control.Monad.Ref
import           Control.Monad.Trans.Class      (MonadTrans, lift)
import           Control.Monad.Trans.Reader     (Reader, ReaderT, runReaderT)
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BL
import           Data.Coerce                    (coerce)
import           Data.Hashable
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Morpheus.Client
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           GHC.IORef
import           "ghcjs-dom" GHCJS.DOM.Document (Document)
import           GHCJS.DOM.Types                (MonadJSM)
import           Network.HTTP.Req
import           Reflex.Dom.Core
import           Reflex.Host.Class
import           System.Random
import           Type.Reflection
import           UnliftIO.MVar

class (Monad m, Reflex t) => HasSource t js m | m -> t js where
  fetchData :: forall query. (Typeable query, Fetch query, Hashable (Args query), FromJSON query) => Event t (Args query) -> m (Event t (Either String query))
  default fetchData :: forall query m' tx. (Typeable query, Fetch query, Hashable (Args query), HasSource t js m', m ~ tx m', MonadTrans tx, FromJSON query) => Event t (Args query) -> m (Event t (Either String query))
  fetchData = lift . fetchData


instance HasSource t js m => HasSource t js (ReaderT r m)
instance HasSource t js m => HasSource t js (EventWriterT t w m)
instance HasSource t js m => HasSource t js (StaticDomBuilderT t m)
instance HasSource t js m => HasSource t js (HydrationDomBuilderT t js m)
instance HasSource t js m => HasSource t js (PostBuildT t m)
instance HasSource t js m => HasSource t js (HydratableT m)
instance (HasSource t js m, ReflexHost t, MonadTrans (PerformEventT t)) => HasSource t js (PerformEventT t m)

type SourceEnv t = (Text, Behavior t (Map (Int, SomeTypeRep) SomeTypeRep))

newtype SourceT t s m a
  = SourceT { unSourceT :: ReaderT (SourceEnv t) m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , DomBuilder t
    , NotReady t
    , MonadIO
    , MonadReader (SourceEnv t)
    , MonadHold t
    , MonadJSM
    , MonadSample t
    , MonadQuery t q
    , MonadFix
    , MonadTrans
    , PostBuild t
    , PerformEvent t
    , Prerender js t
    , TriggerEvent t
    )

instance (Adjustable t m, MonadHold t m) => Adjustable t (SourceT t js m) where
  runWithReplace a e = SourceT $ runWithReplace (coerce a) (coerce <$> e)
  traverseDMapWithKeyWithAdjust f m e = SourceT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = SourceT $ traverseIntMapWithKeyWithAdjust (\k v -> coerce $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = SourceT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) m e

-- type SourceEnv t = (Text, Behavior t (Map (Int, SomeTypeRep) Bool))

instance (MonadIO m, MonadIO (HostFrame t), ReflexHost t, Reflex t, Ref m ~ IORef) => HasSource t js (
  SourceT t js (HydratableT (PostBuildT t (StaticDomBuilderT t (PerformEventT t m)) ))
  ) where

  fetchData :: forall query. (Typeable query, Fetch query, Hashable (Args query), FromJSON query) => Event t (Args query) -> SourceT t js (HydratableT (PostBuildT t (StaticDomBuilderT t (PerformEventT t m)) )) (Event t (Either String query))
  fetchData queryE = do

    (_endpoint, cache) <- ask

    lookupCache :: Map (Int, SomeTypeRep) SomeTypeRep <- sample cache

    performEvent $ do
      coincidence $ ffor queryE $ \(args) -> do
        case Map.lookup (hash args, someTypeRep (Proxy :: Proxy query)) lookupCache of
          Just (rep) -> case typeRep @query `eqTypeRep` (typeOf rep) of
            Just HRefl -> (pure $ Right rep) <$ queryE
            Nothing    -> fetch xhrFetch <$> queryE
          Nothing         -> fetch xhrFetch <$> queryE

    where

      xhrFetch queryBS = do

        runReq defaultHttpConfig $ do
          r <-
            req
            POST -- method
            (http "graphql.localhost") -- safe by construction URL
            (ReqBodyLbs queryBS) -- use built-in options or add your own
            lbsResponse -- specify how to interpret response
            (port 3000) -- query params, headers, explicit port number, etc.
          pure $ responseBody r

instance (Monad m, Reflex t) => HasSource t js (SourceT t js (HydrationDomBuilderT js t m)) where
  fetchData queryE = pure never

-- instance (Monad m, Reflex t, HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m) => HasSource t GhcjsDomSpace (SourceT t GhcjsDomSpace m) where
instance (Monad m, Reflex t, HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m) =>
  HasSource t js (SourceT t js (HydrationDomBuilderT GhcjsDomSpace t m)) where

  fetchData queryE = do
    performEvent $ fetch xhrFetch <$> queryE

    where

      xhrFetch queryBS = do
        (endpoint, _) <- ask

        let req = xhrRequest "POST" endpoint $ def & xhrRequestConfig_sendData .~ BL.toStrict queryBS

        resultVar <- newEmptyMVar
        void $ newXMLHttpRequest req $ liftIO . putMVar resultVar
        resp <- takeMVar resultVar

        let body = case resp ^. xhrResponse_responseText of
              Nothing  -> error "boom"
              Just txt -> BL.fromStrict . T.encodeUtf8 $ txt

        pure body

runSourceT :: SourceEnv t -> SourceT t js m a -> m a
runSourceT cs (SourceT m) = runReaderT m cs
