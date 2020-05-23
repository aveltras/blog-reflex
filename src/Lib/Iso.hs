module Lib.Iso where

import           Control.Monad               (forM_)
import           Control.Monad.Ref
import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BL
import           Data.Dependent.Sum          (DSum (..))
import           Data.Functor.Identity       (Identity (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as TL
import qualified GHCJS.DOM                   as DOM
import qualified GHCJS.DOM.HTMLScriptElement as DOM
import qualified GHCJS.DOM.ParentNode        as DOM
import qualified GHCJS.DOM.Types             as DOM
import           Language.Javascript.JSaddle (JSM)
import           Reflex.Dom.Core
import           Reflex.Host.Class
import           Type.Reflection

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

injectIntoDOM :: forall a t m. (DomBuilder t m, PostBuild t m, ToJSON a, Typeable a) => Dynamic t a -> m ()
injectIntoDOM a = elAttr "script" attrs $ dynText $ (TL.toStrict . encodeToLazyText) <$> a
  where key = T.pack . tyConName . typeRepTyCon $ typeRep @a
        attrs = ("type" =: "text/plain" <> "data-injected" =: key <> skipHydrationAttribute =: "")

extractFromDOM :: forall a. (FromJSON a, Typeable a) => JSM a
extractFromDOM = do

  Just doc <- DOM.currentDocument

  DOM.querySelector doc ("[data-injected=" <> key <> "]" :: Text) >>= \case
    Nothing -> errorMsg
    Just node -> DOM.castTo DOM.HTMLScriptElement node >>= \case
      Nothing -> errorMsg
      Just scriptEl -> decode' @a . BL.fromStrict . T.encodeUtf8 <$> DOM.getText scriptEl >>= \case
        Nothing -> errorMsg
        Just obj -> pure obj

  where key = T.pack . tyConName . typeRepTyCon $ typeRep @a
        errorMsg = error $ "Could not retrieve" <> show key <> "from the DOM."
