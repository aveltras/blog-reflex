module App.Frontend where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy        as BL
import           Data.Map
import qualified Data.Map                    as Map
import           Data.Morpheus.Client
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified GHCJS.DOM                   as DOM
import qualified GHCJS.DOM.Document          as DOM
import qualified GHCJS.DOM.HTMLScriptElement as DOM
import qualified GHCJS.DOM.ParentNode        as DOM
import qualified GHCJS.DOM.Types             as DOM
import           Language.Javascript.JSaddle (JSM, eval)
import           Reflex.Dom.Core
import           Web.PathPieces


import           Lib.Source
import           Lib.View



defineByDocumentFile "schema.graphql" [gql|
  query GetDeity ($goName: String!) {
    deity (name: $goName) {
      name
      power
    }
  }
|]


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


headWidget :: (DomBuilder t m, PostBuild t m, Prerender js t m) => Dynamic t Text -> m ()
headWidget headD = do
  elAttr "script" ("src" =: "http://jsaddle.blog.local:3000/jsaddle.js") blank
  -- elAttr "link" ("rel" =: "stylesheet" <> "href" =: ("http://static.blog.local:3000/" <> main_css)) blank
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

  buildE <- getPostBuild
  responseE :: Event t (Either String GetDeity) <- requesting $ (IsGraphQLQuery (GetDeityArgs "tac")) <$ buildE
  widgetD <- holdDyn "initial" $ ffor responseE $ \r -> case r of
    Left s  -> T.pack $ "Error ---->" <> s
    Right g -> T.pack $ "Success ---> " <> show g

  prerender_ (el "div" $ dynText widgetD) (el "div" $ dynText widgetD)

  blank



mainJS :: JSM ()
mainJS = do

  Just doc <- DOM.currentDocument
  Just hd <- DOM.getHead doc
  t <- DOM.querySelector hd ("[data-prerenderblob]" :: Text) >>= \case
    Nothing -> pure ""
    Just node ->
      DOM.castTo DOM.HTMLScriptElement node >>= \case
        Nothing -> pure ""
        Just e -> T.drop 2 <$> (DOM.getText e) :: JSM Text


  let cacheMap = decode' @(Map Int Text) $ BL.fromStrict $ T.encodeUtf8 t

      cacheMap' = maybe Map.empty (fmap T.encodeUtf8) cacheMap -- doesn't work
      -- cacheMap' = Map.empty -- works

  eval ("console.log('"<>show cacheMap'<>"')" :: String)

  runHydrationWidgetWithHeadAndBody (pure ()) $ \appendHead appendBody -> mdo
    void $ appendHead (headWidget headD)
    ((_, headD), _) <- appendBody $ runSourceT cacheMap' (reflexXhrHandler (def & xhrRequestConfig_withCredentials .~ True)) graphqlCodec $ runDynamicWriterT $ runViewT browserLocHandler $ bodyWidget
    blank
