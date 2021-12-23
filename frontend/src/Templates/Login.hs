module Templates.Login where

import Data.Text (Text)
import Reflex.Dom.Core

import Templates.Partials.Buttons
import Templates.Partials.Containers
import Templates.Types

data LoginMode = LoginMode_Login | LoginMode_Signup

data LoginConfig t m = LoginConfig
  { _loginConfig_mode :: Dynamic t LoginMode
  , _loginConfig_switchModeLink :: Dynamic t Text -> m ()
  , _loginConfig_errors :: Dynamic t (Maybe Text)
  }

data Login t m = Login
  { _login_username :: InputEl t m
  , _login_password :: InputEl t m
  , _login_submit :: Event t ()
  }

login :: Template t m => LoginConfig t m -> m (Login t m)
login cfg = screenContainer $ do
  elClass "div" "p-4 mx-auto md:w-sm t_login" $ mdo
    elClass "h1" "font-karla font-bold text-h1 text-copy mt-12" $ dynText $
      ffor (_loginConfig_mode cfg) $ \case
        LoginMode_Login -> "Login"
        LoginMode_Signup -> "Signup"
    username <- elClass "div" "flex flex-col mt-4" $ do
      elClass "div" "font-facit font-label text-label" $ text "Email"
      inputElement $ def
        & initialAttributes .~ "type" =: "text"
    password <- elClass "div" "flex flex-col mt-8" $ mdo
      elClass "div" "font-facit font-label text-label" $ text "Password"
      inputElement $ def
        & initialAttributes .~ ("type" =: "password")
    maybeDisplay errorMessage $ _loginConfig_errors cfg
    submit <- primaryButton "Log in"
    elClass "div" "font-facit font-label underline text-label text-link text-center mt-4" $
      _loginConfig_switchModeLink cfg $ ffor (_loginConfig_mode cfg) $ \case
        LoginMode_Login -> "Don't have an account?"
        LoginMode_Signup -> "Already have an account?"
    return $ Login
      { _login_username = username
      , _login_password = password
      , _login_submit = submit
      }

maybeDisplay :: Template t m => (a -> m ()) -> Dynamic t (Maybe a) -> m ()
maybeDisplay template val = dyn_ $ ffor val $ \case
  Nothing -> blank
  Just x -> template x

errorMessage :: Template t m => Text -> m ()
errorMessage t = 
  divClass "font-facit text-error text-opacity-70 h-4 mt-1" $ text t
