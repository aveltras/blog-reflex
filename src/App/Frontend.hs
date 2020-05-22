{-# LANGUAGE DeriveAnyClass #-}

module App.Frontend where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO)
import           Data.Aeson
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           GHC.Generics
import           Language.Javascript.JSaddle (JSM)
import           Reflex.Dom.Core
import           Web.PathPieces


import           Lib.Iso
import           Lib.Source
import           Lib.View


import           App.API

data Config = Config
  { configCss    :: Text
  , configScript :: Text
  } deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

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


headWidget :: (DomBuilder t m, PostBuild t m, Prerender js t m) => Config -> Dynamic t Text -> m ()
headWidget Config{..} headD = do
  elAttr "script" ("src" =: configScript) blank -- "http://jsaddle.blog.local:3000/jsaddle.js"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: configCss) blank -- "http://static.blog.local:3000/" <> main_css
  prerender_ (el "title" $ dynText headD) (el "title" $ dynText headD)

bodyWidget :: (DynamicWriter t Text m, MonadIO (Performable m), DomBuilder t m, TriggerEvent t m, HasSource t IsGraphQLQuery m, MonadHold t m, HasView t View ViewError m, PerformEvent t m, Prerender js t m, PostBuild t m) => m ()
bodyWidget = do
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



graphQLwidget :: (MonadIO (Performable m), Prerender js t m, HasSource t IsGraphQLQuery m, TriggerEvent t m, PostBuild t m, MonadHold t m, PerformEvent t m, DomBuilder t m) => m ()
graphQLwidget = do

  text "nothing"
  buildE <- getPostBuild
  responseE :: Event t (Either String GetDeity) <- requesting $ (IsGraphQLQuery (GetDeityArgs "tac")) <$ buildE
  widgetD <- holdDyn "initial" $ ffor responseE $ \r -> case r of
    Left s  -> T.pack $ "Error ---->" <> s
    Right g -> T.pack $ "Success ---> " <> show g

  prerender_ (el "div" $ dynText widgetD) (el "div" $ dynText widgetD)

  blank

mainJS :: JSM ()
mainJS = do
  cacheMap <- extractFromDOM
  config <- extractFromDOM
  runHydrationWidgetWithHeadAndBody (pure ()) $ \appendHead appendBody -> mdo
    void $ appendHead (headWidget config headD)
    ((_, headD), _) <- appendBody $ runSourceT (T.encodeUtf8 <$> cacheMap) (reflexXhrHandler (def & xhrRequestConfig_withCredentials .~ True)) graphqlCodec $ runDynamicWriterT $ runViewT browserLocHandler $ bodyWidget
    blank
