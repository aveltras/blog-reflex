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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Source where

import           Control.Lens
import           Control.Monad                  (void)
import           Control.Monad.Fix              (MonadFix)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Reader.Class     (MonadReader, ask)
import           Control.Monad.Trans.Class      (MonadTrans, lift)
import           Control.Monad.Trans.Reader     (Reader, ReaderT, runReaderT)
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BL
import           Data.Coerce                    (coerce)
import           Data.Hashable
import           Data.Morpheus.Client
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           "ghcjs-dom" GHCJS.DOM.Document (Document)
import           GHCJS.DOM.Types                (MonadJSM)
import           Reflex.Dom.Core
import           Reflex.Host.Class
import           UnliftIO.MVar

class (Monad m, Reflex t) => HasSource t js m | m -> t js where
  fetchData :: (Fetch query, FromJSON query) => Event t (Args query) -> m (Event t (Either String query))
  default fetchData :: (Fetch query, HasSource t s m', m ~ tx m', MonadTrans tx, FromJSON query) => Event t (Args query) -> m (Event t (Either String query))
  fetchData = lift . fetchData

instance HasSource t js m => HasSource t js (ReaderT r m)
instance HasSource t js m => HasSource t js (EventWriterT t w m)
instance HasSource t js m => HasSource t js (StaticDomBuilderT t m)
instance HasSource t js m => HasSource t js (HydrationDomBuilderT t js m)
instance HasSource t js m => HasSource t js (PostBuildT t m)
instance HasSource t js m => HasSource t js (HydratableT m)
instance (HasSource t js m, ReflexHost t, MonadTrans (PerformEventT t)) => HasSource t js (PerformEventT t m)

newtype SourceT t s m a
  = SourceT { unSourceT :: ReaderT Text m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , DomBuilder t
    , NotReady t
    , MonadIO
    , MonadReader Text
    , MonadHold t
    , MonadJSM
    , MonadSample t
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

instance (Monad m, ReflexHost t, Reflex t) => HasSource t js (
  SourceT t js (HydratableT (PostBuildT t (StaticDomBuilderT t (PerformEventT t m)) ))
  ) where
  fetchData queryE = pure never

instance (Monad m, Reflex t) => HasSource t js (SourceT t js (HydrationDomBuilderT js t m)) where
  fetchData queryE = pure never

-- (HasSource
--                          (SpiderTimeline Global)
--                          (JSCtx_JavaScriptCore x)
--                          (SourceT
--                             (SpiderTimeline Global)
--                             js0
--                             (HydrationDomBuilderT
--                                GhcjsDomSpace DomTimeline (DomCoreWidget x))))

-- instance (Monad m, Reflex t) => HasSource t js (SourceT t js m) where
--   fetchData queryE = pure never

-- instance (Monad m, Reflex t, HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m) => HasSource t GhcjsDomSpace (SourceT t GhcjsDomSpace m) where
instance (Monad m, Reflex t, HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m) =>
  HasSource t js (SourceT t js (HydrationDomBuilderT GhcjsDomSpace t m)) where

  fetchData queryE = do
    performEvent $ toPerformable <$> queryE

    where

      toPerformable args = fetch xhrFetch args

      xhrFetch queryBS = do
        endpoint <- ask

        let req = xhrRequest "POST" endpoint $ def & xhrRequestConfig_sendData .~ BL.toStrict queryBS

        resultVar <- newEmptyMVar
        void $ newXMLHttpRequest req $ liftIO . putMVar resultVar
        resp <- takeMVar resultVar

        let body = case resp ^. xhrResponse_responseText of
              Nothing  -> error "boom"
              Just txt -> BL.fromStrict . T.encodeUtf8 $ txt

        pure body

runSourceT :: Text -> SourceT t js m a -> m a
runSourceT cs (SourceT m) = runReaderT m cs

-- graphQLwidget :: (PostBuild t m, MonadHold t m, DomBuilder t m, XhrConstraints t m) => m ()
-- graphQLwidget = do
--   clickE <- button "click"
--   responseE :: Event t (Either String GetDeity) <- xhrQuery (GetDeityArgs "tac" <$ clickE)
--   responseD <- holdDyn "" $ ffor responseE $ \r -> case r of
--     Left s  -> T.pack $ "Error ---->" <> s
--     Right g -> T.pack $ "Success ---> " <> show g
--   display responseD
--   blank


-- type XhrConstraints t m = (HasJSContext (Performable m), MonadJSM (Performable m), PerformEvent t m, HasJSContext m, MonadIO m, MonadJSM m)

-- xhrQuery :: forall query m t. (XhrConstraints t m, FromJSON query, Fetch query) => Event t (Args query) -> m (Event t (Either String query))
-- xhrQuery queryE = performEvent $ toPerformable <$> queryE






-- instance (MonadJSM m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HasSource t (HydrationDomBuilderT GhcjsDomSpace t m) where
--   fetchData = undefined

-- instance HasCookies Snap where
--   askCookies = map (\c -> (cookieName c, cookieValue c)) <$> getsRequest rqCookies



-- instance (Monad m) => HasSource t (StaticDomBuilderT t m) where
--   fetchData = undefined

-- instance (MonadJSM m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HasSource t (HydrationDomBuilderT HydrationDomSpace t m) where
--   fetchData = undefined

-- instance (MonadJSM m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HasSource t (HydrationDomBuilderT GhcjsDomSpace t m) where
--   fetchData = undefined

-- instance HasCookies Snap where
--   askCookies = map (\c -> (cookieName c, cookieValue c)) <$> getsRequest rqCookies

-- instance (MonadJSM m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HasCookies (HydrationDomBuilderT s t m) where
--   askCookies = fmap (parseCookies . encodeUtf8) $ getCookie =<< askDocument


