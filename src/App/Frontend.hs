{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE PartialTypeSignatures #-}

module App.Frontend where

import           Control.Monad               (void)
import           Control.Monad.Fix           (MonadFix)
import           Data.Aeson                  hiding (Success)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           GHC.Generics
import           Language.Javascript.JSaddle (JSM)
import           Reflex.Dom.Core
import           RIO                         hiding (display)
import           Validation
import           Web.PathPieces

import           Lib.Iso
import           Lib.Source
import           Lib.View


import           App.API
import           App.Types

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
  elAttr "script" ("src" =: configScript) blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: configCss) blank
  prerender_ (el "title" $ dynText headD) (el "title" $ dynText headD)

bodyWidget :: forall t m js. (MonadFix m, DynamicWriter t Text m, DomBuilder t m, HasSource t RequestG m, MonadHold t m, HasView t View ViewError m, Prerender js t m, PostBuild t m) => m ()
bodyWidget = do
  viewD <- askView
  void $ dyn $ viewD <&> \case
    Right v -> case v of
      Homepage    -> do
        buildE <- getPostBuild
        tellDyn $ constDyn "home"
        text "home"
        linkTo Contact $ text "go contact"
        clickE <- button "click"
        responseE <- requesting $ RequestG1 <$ leftmost [clickE, buildE]
        dataD <- holdDyn (Left "no response") responseE
        prerender_ (el "div" $ display dataD) (el "div" $ display dataD)

        -- display dataD
        blank
        -- graphQLwidget
      Contact -> mdo
        text "contact"
        tellDyn $ constDyn "contact"
        linkTo Homepage $ text "go home"

        nameI <- inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m)) & inputElementConfig_elementConfig .~ (def & elementConfig_initialAttributes .~ ("tacotac" =: "test"))
        emailI <- el "div" $ inputElement def <* dyn nameErrD
        phoneI <- inputElement def
        messageI <- textAreaElement def

        sendE <- buttonClass "tac" "Send"

        let formD = MessageForm
              <$> _inputElement_value nameI
              <*> _inputElement_value emailI
              <*> _inputElement_value phoneI
              <*> _textAreaElement_value messageI

            messageD = validateMessageForm <$> formD

            nameErrD = messageD <&> \case
              Success _ -> text "success"
              Failure err -> text $ foldMap (T.pack . show) err

        responseE <- requesting $ SendMessage <$> (fmapMaybe id $ successToMaybe <$> tagPromptlyDyn messageD sendE)
        responseD <- holdDyn (Left "no response yet") responseE

        display responseD
        -- display messageD

        blank

    Left e -> case e of
      ViewError -> do
        linkTo Homepage $ text "go home"
        linkTo Contact $ text "go contact"

buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

data MessageForm = MessageForm
  { messageFormName  :: Text
  , messageFormEmail :: Text
  , messageFormPhone :: Text
  , messageFormBody  :: Text
  }

data MessageError
  = EmptyName
  | EmptyEmail
  | EmptyBody
  deriving stock (Show)

validateRequired :: Text -> MessageError -> Validation (NonEmpty MessageError) Text
validateRequired name err = name <$
  failureIf (T.length name < 1) err

validateMessageForm :: MessageForm -> Validation (NonEmpty MessageError) Message
validateMessageForm MessageForm {..} = Message
  <$> validateRequired messageFormName EmptyName
  <*> validateRequired messageFormEmail EmptyEmail
  <*> (Success $ Just messageFormPhone)
  <*> validateRequired messageFormBody EmptyBody

mainJS :: JSM ()
mainJS = do
  cacheMap <- extractFromDOM
  config <- extractFromDOM
  runHydrationWidgetWithHeadAndBody (pure ()) $ \appendHead appendBody -> mdo
    void $ appendHead (headWidget config headD)
    ((_, headD), _) <- appendBody $ runSourceT (T.encodeUtf8 <$> cacheMap) (reflexXhrHandler (def & xhrRequestConfig_withCredentials .~ True)) gadtCodec $ runDynamicWriterT $ runViewT browserLocHandler $ bodyWidget
    blank
