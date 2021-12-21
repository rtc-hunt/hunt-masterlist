module Frontend.Templates.Partials.ChannelList where

import Control.Category
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Data.Bool
import Data.Default
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vessel.Vessel
import Obelisk.Route.Frontend
import Prelude hiding ((.), id)
import Reflex.Dom.Core
import Rhyolite.Api hiding (Request)
import Rhyolite.Vessel.Path

import Common.Request
import Common.Route
import Common.View

import Frontend.Templates.Partials.Buttons
import Frontend.Templates.Partials.Headers
import Frontend.Templates.Partials.Lists
import Frontend.Templates.Partials.Searchbar
import Frontend.Utils


data ChannelListConfig t = ChannelListConfig
  { _channelListConfig_headerClasses :: T.Text
  , _channelListConfig_useH2 :: Bool
  }

instance Default (ChannelListConfig t) where
  def = ChannelListConfig "" False

channelList
  :: ( MonadFix m
     , MonadHold t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , PostBuild t m
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () publicRequest PrivateRequest
     )
  => ChannelListConfig t
  -> m (Event t ())
channelList (ChannelListConfig headerClasses useH2) = do
  elClass "div" "flex-grow p-4" $ do
    createClick <- elClass "div" (classList ["w-full flex flex-row items-center justify-between", headerClasses]) $ do
      header' $ def
        & headerConfig_header .~ "Channels"
      iconButton "add"
    channelSearch <- searchbar "Search for a channel, or create a new one"

    elClass "div" "mt-8 font-facit text-h2 text-copy" $ text "Search results"

    mRooms <- maybeDyn <=< watch . ffor (_searchbarOutput_query channelSearch) $ \q ->
      key V_Chatrooms ~> key (ChatroomQuery q) ~> semiMapP
    channelClick <- (switchHold never =<<) $ dyn $ ffor mRooms $ \case
      Nothing -> pure never
      Just rooms -> switchDyn . fmap mergeMap <$> list rooms channelItem

    _ <- fmap fanEither . requestingIdentity . ffor (tag (current (_searchbarOutput_query channelSearch)) createClick) $ \newName ->
      ApiRequest_Private () $ PrivateRequest_CreateChatroom newName

    setRoute $ fforMaybe channelClick $ \clicks -> ffor (Map.minViewWithKey clicks) $ \((k,_),_) ->
      FrontendRoute_Channel :/ k

    pure ()

  -- TODO(skylar): This could be its own component but currently it always appears with the channel list
  elClass "div" "w-full p-4 border-t border-metaline bg-white md:bg-less-sunken" $ do
    elClass "div" "w-full flex flex-row items-center" $ do
      elClass "div" "w-12 h-12 rounded-full bg-primary-darker mr-2 flex-shrink-0" blank
      elClass "div" "flex flex-col font-facit text-label h-full flex-grow" $ do
        elClass "div" "text-copy" $ text "Sky"
        elClass "div" "text-light" $ text "soquinn@obsidian.systems"

    secondaryButton "mt-4" "Logout"
  where
    header' = bool h1 h2 useH2

channelItem
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Text
  -> m (Event t ())
channelItem = listItem $ def
  { _listItemConfig_clickable = True
  }

makeLenses ''ChannelListConfig
