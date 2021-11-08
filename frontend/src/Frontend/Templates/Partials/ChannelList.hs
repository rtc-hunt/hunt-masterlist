{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This is kind of a hefty widget, not just the channel list but your access to settings and what not

module Frontend.Templates.Partials.ChannelList where

import Data.Bool
import Data.Default
import qualified Data.Text as T

import Reflex.Dom.Core

import Frontend.Utils

import Common.Route
import Obelisk.Route.Frontend

import Control.Lens
import Control.Monad

import Frontend.Templates.Partials.Searchbar
import Frontend.Templates.Partials.Headers
import Frontend.Templates.Partials.Buttons
import Frontend.Templates.Partials.Lists

data ChannelListConfig t = ChannelListConfig
  { _channelListConfig_headerClasses :: T.Text
  , _channelListConfig_useH2 :: Bool
  }

instance Default (ChannelListConfig t) where
  def = ChannelListConfig "" False

channelList :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Prerender js t m) => ChannelListConfig t -> m ()
channelList (ChannelListConfig headerClasses useH2) = do
  elClass "div" "flex-grow p-4" $ do
    elClass "div" (classList ["w-full flex flex-row items-center justify-between", headerClasses]) $ do
      header $ def
        & headerConfig_header .~ "Channels"
      iconButton "add"
    searchbar "Search for a channel"

    elClass "div" "mt-8 font-facit text-h2 text-copy" $ text "Recent channels"

    replicateM_ 4 channelItem

  -- TODO(skylar): This could be its own component but currently it always appears with the channel list
  elClass "div" "w-full p-4 border-t border-metaline bg-white md:bg-less-sunken" $ do
    elClass "div" "w-full flex flex-row items-center" $ do
      elClass "div" "w-12 h-12 rounded-full bg-primary-darker mr-2 flex-shrink-0" blank
      elClass "div" "flex flex-col font-facit text-label h-full flex-grow" $ do
        elClass "div" "text-copy" $ text "Sky"
        elClass "div" "text-light" $ text "soquinn@obsidian.systems"

      secondaryIconButton "" "settings"
    secondaryButton "mt-4" "Logout"
  where
    header = bool h1 h2 useH2

-- TODO(skylar): Is this just a link?
channelItem :: DomBuilder t m => m ()
channelItem = listItem "#ChannelName" $ Just "543 members \x00b7 13 online"

makeLenses ''ChannelListConfig
