{-# LANGUAGE CPP #-}

module Lib.Source where

import           Control.Lens           hiding (has)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import           Data.Aeson.Parser      (decodeWith, json')
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Coerce            (coerce)
import           Data.Constraint.Extras (Has, has, whichever)
import           Data.Constraint.Forall (ForallF)
import           Data.Hashable
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Text.Encoding     as T
import           GHCJS.DOM.Types        (MonadJSM)
import           Reflex.Dom.Core        hiding (Error, Query, Value)


type HasSource t request m =
  ( Requester t (Client m)
  , Requester t m
  , Request (Client m) ~ request
  , Request m ~ request
  , Response (Client m) ~ Either String
  , Response m ~ Either String
  )

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
#ifndef ghcjs_HOST_OS
    , MonadJSM
#endif
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
  ( MonadFix m
  , MonadHold t m
  , Hashable wireFormat
  -- , Show wireFormat
  , PerformEvent t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  -- , PostBuild t m
  ) => Map Int wireFormat
  -> (Event t (Map Int wireFormat) -> m (Event t (Map Int wireFormat)))
  -> (forall b. request b -> (wireFormat, wireFormat -> response b))
  -> SourceT t request response m a
  -> m (a, Dynamic t (Map Int wireFormat))
runSourceT cacheMap requestHandler codec (SourceT widget) = mdo

  cacheD :: Dynamic t (Map Int wireFormat) <- accumDyn (\a b -> a <> (Map.fromList . fmap (\(_,v) -> (hash v, v)) . Map.toList $ b)) cacheMap $ wireResponsesE
  -- hash request and not response !

  let partitionedRequestsE = partitionRequests <$> (attachPromptlyDyn cacheD wireRequestsE)
      wireRequestsNotCachedE = ffor (partitionedRequestsE) snd

  cachedResponsesE <- delay 0.000001 $ ffor partitionedRequestsE fst

  wireResponsesE <- requestHandler $ wireRequestsNotCachedE
  (result, requestE) <- runRequesterT widget responseE
  (wireRequestsE, responseE) <- matchResponsesWithRequests codec requestE $ fmapMaybe id (safeHead . Map.toList <$> (wireResponsesE <> cachedResponsesE))

  pure (result, cacheD)

    where
      partitionRequests :: (Map Int wireFormat, Map Int wireFormat) -> (Map Int wireFormat, Map Int wireFormat)
      partitionRequests (cache, requests) = Map.foldrWithKey f (Map.empty, Map.empty) requests
        where
          f k a (x1, x2) = case Map.lookup (hash a) cache of
            Nothing -> (x1, Map.insert k a x2)
            Just bs -> (Map.insert k bs x1, x2)


safeHead :: [a] -> Maybe a
safeHead = \case
  [] -> Nothing
  x:_ -> Just x

type WireFormat = BS.ByteString

gadtCodec :: forall request response. (ForallF ToJSON request, Has FromJSON request) => request response -> (WireFormat, WireFormat -> Either String response)
gadtCodec request = (toWire, fromWire)
  where
    toWire = BL.toStrict . encode $ whichever @ToJSON @request @response toJSON request
    fromWire wire =
      case decodeWith json' fromJSON $ BL.fromStrict wire of
        Nothing -> Left "error"
        Just s -> case has @FromJSON request fromJSON s of
          Error err         -> Left err
          Success (Left a)  -> Left a
          Success (Right a) -> Right a

reflexXhrHandler :: (XhrConstraints t m) => XhrRequestConfig () -> Event t (Map Int WireFormat) -> m (Event t (Map Int WireFormat))
reflexXhrHandler xhrConfig requestsE = do
  xhrResponsesE <- performRequestsAsync $ (fmap . fmap) toXhrRequest requestsE
  pure $ (fmap . fmap) extractBody xhrResponsesE

    where

      toXhrRequest :: WireFormat -> XhrRequest WireFormat
      toXhrRequest wire = xhrRequest "POST" "http://graphql.blog.local:3000" $ xhrConfig & xhrRequestConfig_sendData .~ wire

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
