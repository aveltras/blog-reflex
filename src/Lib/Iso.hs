module Lib.Iso where

import           Control.Monad           (forM_)
import           Control.Monad.Ref
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Dependent.Sum      (DSum (..))
import           Data.Functor.Identity   (Identity (..))
import           Reflex.Dom.Core
import           Reflex.Host.Class

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
