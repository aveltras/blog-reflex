{-# LANGUAGE AllowAmbiguousTypes   #-}
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

data View
  = HomeP
  | ContactP
  | LoginP
  | BlogP
  | BlogPostP Text
  | PageP Text
  | AdminP AdminView
  deriving (Show)

instance PathMultiPiece View where

  fromPathMultiPiece = \case
    [] -> Just HomeP
    ["contact"] -> Just ContactP
    ["login"] -> Just LoginP
    ["blog"] -> Just BlogP
    ["blog", slug] -> Just $ BlogPostP slug
    "admin":x -> AdminP <$> fromPathMultiPiece x
    [slug] -> Just $ PageP slug
    _ -> Nothing

  toPathMultiPiece = \case
    HomeP -> []
    ContactP -> ["contact"]
    LoginP -> ["login"]
    BlogP -> ["blog"]
    BlogPostP slug -> ["blog", slug]
    PageP slug -> [slug]
    AdminP a -> "admin" : toPathMultiPiece a


data AdminView
  = AdminDashboardP
  | AdminBlogP
  | AdminPageP
  | AdminMessageP
  deriving (Show)

instance PathMultiPiece AdminView where

  fromPathMultiPiece = \case
    [] -> Just AdminDashboardP
    ["blog"] -> Just AdminBlogP
    ["page"] -> Just AdminPageP
    ["message"] -> Just AdminMessageP
    _ -> Nothing

  toPathMultiPiece = \case
    AdminDashboardP -> []
    AdminBlogP -> ["blog"]
    AdminPageP -> ["page"]
    AdminMessageP -> ["message"]

type AppWidget t js m =
  ( MonadFix m
  , DynamicWriter t Text m
  , DomBuilder t m
  , HasSource t RequestG m
  , MonadHold t m
  , HasView t View ViewError m
  , Prerender js t m
  , PostBuild t m
  )

headWidget :: (DomBuilder t m, PostBuild t m, Prerender js t m) => Config -> Dynamic t Text -> m ()
headWidget Config{..} headD = do
  elAttr "script" ("src" =: configScript) blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: configCss) blank
  prerender_ (el "title" $ dynText headD) (el "title" $ dynText headD)

bodyWidget :: forall t m js. (MonadFix m, DynamicWriter t Text m, DomBuilder t m, HasSource t RequestG m, MonadHold t m, HasView t View ViewError m, Prerender js t m, PostBuild t m) => m ()
bodyWidget = do
  elClass "header" "bg-red-500 text-white p-4" $ do
    elClass "nav" "flex items-center justify-between container mx-auto" $ nav
  elClass "div" "container mx-auto" $ do
    viewD <- askView
    void $ dyn $ viewD <&> \case
      Right v -> case v of
        HomeP          -> homepage
        ContactP       -> contact
        LoginP         -> login
        BlogP          -> blog
        BlogPostP slug -> blogPost slug
        PageP slug     -> page slug
        AdminP p -> do

          linkTo (AdminP AdminDashboardP) $ text "Dashboard"
          linkTo (AdminP AdminBlogP) $ text "Blog"
          linkTo (AdminP AdminPageP) $ text "Page"
          linkTo (AdminP AdminMessageP) $ text "Messages"

          case p of
            AdminDashboardP -> text "dashboard"
            AdminBlogP      -> text "admin - Blog"
            AdminPageP      -> text "admin - Pages"
            AdminMessageP   -> text "admin - Messages"

      Left e -> case e of
        ViewError -> do
          el "h1" $ text "404"

nav :: AppWidget t js m => m ()
nav = do
  linkTo HomeP $ text "Home"
  linkTo ContactP $ text "Contact"
  linkTo LoginP $ text "Login"
  linkTo BlogP $ text "Blog"
  linkTo (BlogPostP "tac") $ text "Blog Post"
  linkTo (PageP "page") $ text "Page"
  linkTo (AdminP AdminDashboardP) $ text "Dashboard"

homepage :: AppWidget t js m => m ()
homepage = do
  el "h1" $ text "Homepage"
  buildE <- getPostBuild
  tellDyn $ constDyn "home"
  clickE <- button "click"
  blank
  -- responseE <- requesting $ RequestG1 <$ leftmost [clickE, buildE]
  -- dataD <- holdDyn (Left "no response") responseE
  -- prerender_ (el "div" $ display dataD) (el "div" $ display dataD)

contact :: forall t js m. AppWidget t js m => m ()
contact = mdo
  el "h1" $ text "Contact"
  tellDyn $ constDyn "contact"

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

login :: forall t js m. AppWidget t js m => m ()
login = do
  el "h1" $ text "Login"
  elAttr "label" ("for" =: "email") $ text "Email"
  emailI <- inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m)) & inputElementConfig_elementConfig .~ (def & elementConfig_initialAttributes .~ ("class" =: "border" <> "id" =: "email"))
  elAttr "label" ("for" =: "password") $ text "Password"
  passwordI <- inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m)) & inputElementConfig_elementConfig .~ (def & elementConfig_initialAttributes .~ ("type" =: "password" <> "id" =: "password"))
  elClass "button" "bg-red-500 text-white py-2 px-4 rounded font-bold" $ text "Login"
  blank

blog :: AppWidget t js m => m ()
blog = el "h1" $ text "Blog"

blogPost :: AppWidget t js m => Text -> m ()
blogPost slug = el "h1" $ text $ "Blog Post " <> slug

page :: AppWidget t js m => Text -> m ()
page slug = do
  buildE <- getPostBuild
  setError $ ViewError <$ buildE
  el "h1" $ text $ "Page " <> slug




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
