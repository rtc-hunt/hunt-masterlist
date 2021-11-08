{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Templates.Partials.Switch where

import Data.Bool
import Data.Default

import Control.Lens

import Frontend.Utils
import Reflex.Dom.Core
import qualified Data.Text as T

data SwitchConfig t = SwitchConfig
  { _switchConfig_label :: T.Text
  , _switchConfig_icon :: Maybe T.Text
  , _switchConfig_classes :: T.Text
  , _switchConfig_disabled :: Dynamic t Bool
  }

instance Reflex t => Default (SwitchConfig t) where
  def = SwitchConfig "" Nothing "" (pure False)

switchButton :: (PostBuild t m, DomBuilder t m) => SwitchConfig t -> m ()
switchButton (SwitchConfig label mIcon cs dEnabled) = do
  elClass "div" (classList ["w-full flex flex-row justify-between items-center", cs]) $ do
    elDynClass "div" (mkTextClasses <$> dEnabled) $ do
      elClass "div" "font-icon leading-none text-icon text-copy mr-1 " $ maybe blank text mIcon
      elClass "div" "font-facit text-body text-copy" $ text label

    elDynClass "div" (mkContainerClasses <$> dEnabled) $ do
      elDynClass "div" (mkSwitchClasses <$> dEnabled) blank
  where
    mkTextClasses b =
      classList [ "flex flex-row items-center"
                , bool "" "opacity-50" b
                ]

    mkContainerClasses b =
      classList [ "rounded-small w-16 h-8 bg-inset p-0.3 flex flex-row justify-end"
                , bool "shadow-input" "" b
                ]

    mkSwitchClasses b =
      classList [ "bg-white w-1/2 h-1/2 rounded-small"
                , bool "shadow-small" "" b
                ]

makeLenses ''SwitchConfig
