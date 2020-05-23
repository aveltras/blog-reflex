{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE PartialTypeSignatures #-}

module App.Frontend where

import           Control.Monad               (void)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.IO.Class      (MonadIO)
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

bodyWidget :: forall t m js. (MonadFix m, DynamicWriter t Text m, MonadIO (Performable m), DomBuilder t m, TriggerEvent t m, HasSource t IsGraphQLQuery m, MonadHold t m, HasView t View ViewError m, PerformEvent t m, Prerender js t m, PostBuild t m) => m ()
bodyWidget = do
  viewD <- askView
  void $ dyn $ viewD <&> \case
    Right v -> case v of
      Homepage    -> do
        tellDyn $ constDyn "home"
        text "home"
        linkTo Contact $ text "go contact"
        graphQLwidget
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

        responseE :: Event t (Either String SendMessage) <- requesting $ (IsGraphQLQuery . SendMessageArgs) <$> (fmapMaybe id $ successToMaybe <$> tagPromptlyDyn messageD sendE)
        responseD <- holdDyn (Left "no response yet") responseE

        display responseD
        display messageD

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

mkMessage :: (Text, Text, Text, Text) -> Either (Text, Text, Text, Text) Message
mkMessage (name, email, phone, body) = do
  -- Left (name, email, phone, body)
  pure $ Message name email (Just phone) body


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
