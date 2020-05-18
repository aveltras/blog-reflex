{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module View where

import           Control.Lens                  hiding (element)
import           Control.Monad.Fix             (MonadFix)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Class     (MonadTrans, lift)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Coerce                   (coerce)
import           Data.Proxy
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified GHCJS.DOM                     as DOM
import qualified GHCJS.DOM.EventM              as DOM
import qualified GHCJS.DOM.History             as DOM
import qualified GHCJS.DOM.Window              as DOM
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import           Language.Javascript.JSaddle   (MonadJSM)
import           Reflex.Dom.Core
import           Reflex.Host.Class             (MonadReflexCreateTrigger,
                                                ReflexHost)
import           Web.PathPieces


class (Monad m, PathPiece view) => HasView t view err m | m -> view, m -> err, m -> t where

  askView :: m (Dynamic t (Either err view))
  default askView :: (HasView t view err m', m ~ tx m', MonadTrans tx)  => m (Dynamic t (Either err view))
  askView = lift askView

  setView :: Event t view -> m ()
  default setView :: (HasView t view err m', m ~ tx m', MonadTrans tx) => Event t view -> m ()
  setView = lift . setView

  setError :: Event t err -> m ()
  default setError :: (HasView t view err m', m ~ tx m', MonadTrans tx) => Event t err -> m ()
  setError = lift . setError


-- instance (MonadJSM m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HasView t view err (HydrationDomBuilderT s t m) where
  -- askView = fmap (parseCookies . encodeUtf8) $ getCookie =<< askDocument

instance (HasView t view err m) => HasView t view err (StaticDomBuilderT t m)
instance (HasView t view err m) => HasView t view err (PostBuildT t m)
instance (HasView t view err m) => HasView t view err (HydrationDomBuilderT s t m)
instance (HasView t view err m, ReflexHost t, MonadTrans (PerformEventT t)) => HasView t view err (PerformEventT t m)
-- instance (HasView t view err m) => HasView t view err (WithJSContextSingleton x m)

-- -- instance (HasRoute t r m) => HasRoute t r (BehaviorWriterT t w m)
-- -- instance (HasRoute t r m) => HasRoute t r (DynamicWriterT t w m)
-- -- instance (HasRoute t r m) => HasRoute t r (EventWriterT t w m)
-- -- instance (HasRoute t r m, ReflexHost t, MonadTrans (PerformEventT t)) => HasRoute t r (PerformEventT t m)
-- -- instance (HasRoute t r m) => HasRoute t r (PostBuildT t m)
-- -- instance (HasRoute t r m) => HasRoute t r (QueryT t q m)
-- -- instance (HasRoute t r m) => HasRoute t r (ReaderT r m)
-- -- instance (HasRoute t r m) => HasRoute t r (RequesterT t request response m)
-- -- instance (HasRoute t r m) => HasRoute t r (TriggerEventT t m)

newtype ViewT t view err m a
  = ViewT { unViewT :: ReaderT (Dynamic t (Either err view)) (EventWriterT t (Either err view) m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , DomBuilder t
    , NotReady t
    , PostBuild t
    , PerformEvent t
    , HasJSContext
    )

-- instance PerformEvent t m => PerformEvent t (ViewT t view err m) where
--   type Performable (ViewT t view err m) = Performable m
--   performEvent = lift . performEvent
--   performEvent_ = lift . performEvent_

--     ( Functor
--     , Applicative
--     , Monad
--     , MonadFix
--     , DomBuilder t
--     , DomRenderHook t
--     , HasDocument
--     , HasJSContext
--     , MonadReflexCreateTrigger t
--     , MonadIO
-- #ifndef ghcjs_HOST_OS
--     , MonadJSM
-- #endif
--     , MonadHold t
--     , MonadSample t
--     , NotReady t
--     , PerformEvent t
--     , PostBuild t
--     , TriggerEvent t
--     )


instance (Adjustable t m, MonadHold t m) => Adjustable t (ViewT t view err m) where
  runWithReplace a e = ViewT $ runWithReplace (coerce a) (coerce <$> e)
  traverseDMapWithKeyWithAdjust f m e = ViewT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = ViewT $ traverseIntMapWithKeyWithAdjust (\k v -> coerce $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = ViewT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) m e

instance (Monad m, PathPiece view, Reflex t) => HasView t view err (ViewT t view err m) where
  askView = ViewT ask
  setView = ViewT . tellEvent . fmap Right
  setError = ViewT . tellEvent . fmap Left

instance MonadTrans (ViewT t view err) where
  lift = undefined

type LocationHandler t m = Event t Text -> m (Text, Event t Text)

data ViewError = ViewError

runViewT ::
  ( Monad m
  , err ~ ViewError
  , MonadFix m
  , MonadHold t m
  , PathPiece view
  , Reflex t
  ) => LocationHandler t m -> ViewT t view err m a -> m (Dynamic t (Either err view))
runViewT locHandler (ViewT m) = mdo
  (initialPath, locationE) <- locHandler $ (fmap ((<>) "/". toPathPiece) . snd . fanEither) viewE
  viewD <- holdDyn (decodeLoc initialPath) $ leftmost [decodeLoc <$> locationE, viewE]
  (_result, viewE) <- runEventWriterT $ runReaderT m viewD
  pure viewD
  where decodeLoc t = maybe (Left ViewError) Right $ fromPathPiece $ T.dropWhile ((==) '/' )t

constLocHandler :: (Monad m, Reflex t) => Text -> LocationHandler t m
constLocHandler path = pure . const (path, never)

browserLocHandler :: (Monad m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => LocationHandler t m
browserLocHandler internalLocE = mdo
  window <- DOM.currentWindowUnchecked
  history <- DOM.getHistory window
  locationE <- wrapDomEvent window (`DOM.on` DOM.popState) getLocationPath
  performEvent_ $ ffor internalLocE $ DOM.pushState history (0 :: Double) ("" :: Text) . Just
  (,) <$> getLocationPath <*> (pure $ leftmost [internalLocE, locationE])

linkTo :: forall t m a view err. (DomBuilder t m, HasView t view err m) => view -> m a -> m a
linkTo v w = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault)
        & elementConfig_initialAttributes .~ "href" =: ("/" <> toPathPiece v)
  (e, a) <- element "a" cfg w
  setView $ v <$ domEvent Click e
  pure a






-- browserHistoryManager :: (MonadJSM m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => Event t Text -> m (Text, Event t Text)
-- browserHistoryManager internalRouteE = mdo
--   window <- DOM.currentWindowUnchecked
--   history <- DOM.getHistory window
--   locationE <- wrapDomEvent window (`DOM.on` DOM.popState) getLocationPath
--   performEvent_ $ ffor internalRouteE $ DOM.pushState history (0 :: Double) ("" :: Text) . Just
--   path <- getLocationPath
--   pure $ (path, leftmost [internalRouteE, locationE])


-- runRouteT :: forall route t m a.
--   ( Monad m
--   , MonadFix m
--   , MonadHold t m
--   , Reflex t
--   , Route route
--   ) => (Event t Text -> m (Text, Event t Text)) -> RouteT t route m a -> m (Dynamic t (Either ViewError route))
-- runRouteT manager (RouteT m) = mdo
--   (initialPath, locationE) <- manager $ fmap encodeLocation $ (snd . fanEither) routeE
--   routeD <- holdDyn (decodeLocation initialPath) $ leftmost [decodeLocation <$> locationE, routeE]
--   (result, routeE) <- runEventWriterT $ runReaderT m $ routeD
--   pure routeD

--   where

--     decodeLocation :: Text -> Either ViewError route
--     decodeLocation t = case decode . dropWhile ((==) "") . T.splitOn "/" $ t of
--       Nothing -> Left NotFound
--       Just v  -> Right v

--     encodeLocation :: route -> Text
--     encodeLocation = T.intercalate "/" . encode


-- constHistoryManager :: (Monad m, Reflex t) => Text -> Event t Text -> m (Text, Event t Text)
-- constHistoryManager path = pure . const (path, never)

-- newtype RouteT t route m a
--   = RouteT { unRouteT :: ReaderT (Dynamic t (Either ViewError route)) (EventWriterT t (Either ViewError route) m) a}
--   deriving
--     ( Functor
--     , Applicative
--     , Monad
--     , DomBuilder t
--     , NotReady t
--     , PostBuild t
--     )

-- instance (Monad m, Reflex t, Route route) => HasRoute t route (RouteT t route m) where
--   askRoute = RouteT ask
--   setRoute = RouteT . tellEvent . fmap Right
--   setError = RouteT . tellEvent . fmap Left
--   showRoute = pure . (<>) "/" . T.intercalate "/" . encode

-- --   deriving
-- --     ( Functor
-- --     , Applicative
-- --     , Monad
-- --     , MonadFix
-- --     , DomBuilder t
-- --     , DomRenderHook t
-- --     , HasDocument
-- --     , HasJSContext
-- --     , MonadReflexCreateTrigger t
-- --     , MonadIO
-- -- #ifndef ghcjs_HOST_OS
-- --     , MonadJSM
-- -- #endif
-- --     , MonadHold t
-- --     , MonadSample t
-- --     , NotReady t
-- --     , PerformEvent t
-- --     , PostBuild t
-- --     , TriggerEvent t
-- --     )



-- class (Monad m, Route route) => HasRoute t route m | m -> route, m -> t where

--   askRoute :: m (Dynamic t (Either ViewError route))
--   default askRoute :: (HasRoute t route m', m ~ tx m', MonadTrans tx) => m (Dynamic t (Either ViewError route))
--   askRoute = lift askRoute

--   setRoute :: Event t route -> m ()
--   default setRoute :: (HasRoute t route m', m ~ tx m', MonadTrans tx) => Event t route -> m ()
--   setRoute = lift . setRoute

--   setError :: Event t ViewError -> m ()
--   default setError :: (HasRoute t route m', m ~ tx m', MonadTrans tx) => Event t ViewError -> m ()
--   setError = lift . setError

--   showRoute :: route -> m Text
--   default showRoute :: (HasRoute t route m', m ~ tx m', MonadTrans tx) => route -> m Text
--   showRoute = lift . showRoute




-- data ViewError = NotFound
--                | Unauthorized

-- -- runRouteT :: (Monad m, MonadFix m, MonadHold t m, PostBuild t m, Reflex t, Semigroup r) => RouteConfig t r m -> RouteT t r m a -> m a

-- runRouteT :: forall route t m a.
--   ( Monad m
--   , MonadFix m
--   , MonadHold t m
--   , Reflex t
--   , Route route
--   ) => (Event t Text -> m (Text, Event t Text)) -> RouteT t route m a -> m (Dynamic t (Either ViewError route))
-- runRouteT manager (RouteT m) = mdo
--   (initialPath, locationE) <- manager $ fmap encodeLocation $ (snd . fanEither) routeE
--   routeD <- holdDyn (decodeLocation initialPath) $ leftmost [decodeLocation <$> locationE, routeE]
--   (result, routeE) <- runEventWriterT $ runReaderT m $ routeD
--   pure routeD

--   where

--     decodeLocation :: Text -> Either ViewError route
--     decodeLocation t = case decode . dropWhile ((==) "") . T.splitOn "/" $ t of
--       Nothing -> Left NotFound
--       Just v  -> Right v

--     encodeLocation :: route -> Text
--     encodeLocation = T.intercalate "/" . encode


-- constHistoryManager :: (Monad m, Reflex t) => Text -> Event t Text -> m (Text, Event t Text)
-- constHistoryManager path = pure . const (path, never)

-- browserHistoryManager :: (MonadJSM m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => Event t Text -> m (Text, Event t Text)
-- browserHistoryManager internalRouteE = mdo
--   window <- DOM.currentWindowUnchecked
--   history <- DOM.getHistory window
--   locationE <- wrapDomEvent window (`DOM.on` DOM.popState) getLocationPath
--   performEvent_ $ ffor internalRouteE $ DOM.pushState history (0 :: Double) ("" :: Text) . Just
--   path <- getLocationPath
--   pure $ (path, leftmost [internalRouteE, locationE])

-- linkTo :: forall t m a r. (DomBuilder t m, HasRoute t r m) => r -> m a -> m a
-- linkTo r w = do
--   loc <- showRoute r
--   let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
--         & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault)
--         & elementConfig_initialAttributes .~ "href" =: loc
--   (e, a) <- element "a" cfg w
--   setRoute $ r <$ domEvent Click e
--   return a
