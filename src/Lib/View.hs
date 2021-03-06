module Lib.View where

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
import           Reflex.Host.Class
import           Web.PathPieces


class (Monad m, PathMultiPiece view) => HasView t view err m | m -> view, m -> err, m -> t where

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
instance (HasView t view err m) => HasView t view err (HydratableT m)
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
    , MonadFix
    , MonadIO
    , MonadHold t
    , MonadSample t
    -- , HasSource t js
    , TriggerEvent t
    , DynamicWriter t w
    , DomBuilder t
    , NotReady t
    , PostBuild t
    , PerformEvent t
    , HasJSContext
    , Requester t
    , Prerender js t
    )

instance (Adjustable t m, MonadHold t m) => Adjustable t (ViewT t view err m) where
  runWithReplace a e = ViewT $ runWithReplace (coerce a) (coerce <$> e)
  traverseDMapWithKeyWithAdjust f m e = ViewT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = ViewT $ traverseIntMapWithKeyWithAdjust (\k v -> coerce $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = ViewT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) m e

instance (Monad m, PathMultiPiece view, Reflex t) => HasView t view err (ViewT t view err m) where
  askView = ViewT ask
  setView = ViewT . tellEvent . fmap Right
  setError = ViewT . tellEvent . fmap Left

instance MonadTrans (ViewT t view err) where
  lift = undefined

type LocationHandler t m = Event t Text -> m (Text, Event t Text)

data ViewError = ViewError
  deriving (Show)

runViewT :: forall t m err view a.
  ( Monad m
  , err ~ ViewError
  , MonadFix m
  , MonadHold t m
  , PathMultiPiece view
  , Show err
  , Show view
  , Reflex t
  ) => LocationHandler t m -> ViewT t view err m a -> m (Dynamic t (Either err view))
runViewT locHandler (ViewT m) = mdo
  (initialPath, locationE) <- locHandler $ (fmap ((<>) "/" . T.intercalate "/" . toPathMultiPiece) . snd . fanEither) viewE
  viewD <- holdDyn (decodeLoc $ T.splitOn "/" initialPath) $ leftmost [decodeLoc <$> (T.splitOn "/" <$> locationE), viewE]
  (_result, viewE) <- runEventWriterT $ runReaderT m viewD
  pure viewD
  where
    decodeLoc :: [Text] -> Either ViewError view
    decodeLoc t = maybe (Left ViewError) Right $ fromPathMultiPiece $ dropWhile ((==) "") t

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
        & elementConfig_initialAttributes .~ "href" =: ("/" <> (T.concat $ toPathMultiPiece v))
  (e, a) <- element "a" cfg w
  setView $ v <$ domEvent Click e
  pure a
