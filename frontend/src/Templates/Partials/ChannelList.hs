module Templates.Partials.ChannelList where

import Control.Category
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Data.Bool
import Data.Default
import qualified Data.Map as Map
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

import Templates.Types
import Templates.Partials.Buttons
import Templates.Partials.Headers
import Templates.Partials.Lists
import Templates.Partials.Searchbar
import Frontend.Utils

data ChannelListConfig m = ChannelListConfig
  { _channelListConfig_resultDisplay :: m ()
  }

data ChannelList t m = ChannelList
  { _channelList_input :: InputEl t m
  , _channelList_add :: Event t ()
  }

channelList :: (DomBuilder t m) => ChannelListConfig m -> m (ChannelList t m)
channelList cfg = do
  elClass "div" "flex-shrink-0 w-1/4 h-full flex-col bg-sunken hidden md:flex border-r border-metaline" $
    elClass "div" "flex-grow p-4" $ do
      addClick <- elClass "div" (classList ["w-full flex flex-row items-center justify-between mt-6"]) $ do
        h2 $ text "Channels"
        iconButton "add"
      search <- searchbar "Search for a channel, or create a new one"

      elClass "div" "mt-8 font-facit text-h2 text-copy" $ text "Search results"
      _channelListConfig_resultDisplay cfg
      return (ChannelList { _channelList_input = search, _channelList_add = addClick })

channelSearchResults
  :: ( MonadFix m
     , MonadHold t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , PostBuild t m
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () publicRequest PrivateRequest
     )
  => ChannelList t m
  -> m ()
channelSearchResults cs = do
    mRooms <- maybeDyn <=< watch . ffor (value (_channelList_input cs)) $ \q ->
      key V_Chatrooms ~> key (ChatroomQuery q) ~> semiMapP

    channelClick <- (switchHold never =<<) $ dyn $ ffor mRooms $ \case
      Nothing -> pure never
      Just rooms -> switchDyn . fmap mergeMap <$> list rooms channelItem

    _ <- fmap fanEither . requestingIdentity . ffor (tag (current (value (_channelList_input cs))) (_channelList_add cs)) $ \newName ->
      ApiRequest_Private () $ PrivateRequest_CreateChatroom newName

    setRoute $ fforMaybe channelClick $ \clicks -> ffor (Map.minViewWithKey clicks) $ \((k,_),_) ->
      FrontendRoute_Channel :/ Just k

channelItem
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Text
  -> m (Event t ())
channelItem = listItem $ def
  { _listItemConfig_clickable = True
  }

userDisplay :: (DomBuilder t m) => Text -> Text -> m ()
userDisplay name email = do
  elClass "div" "w-full p-4 border-t border-metaline bg-white md:bg-less-sunken" $ do
  elClass "div" "w-full flex flex-row items-center" $ do
    elClass "div" "w-12 h-12 rounded-full bg-primary-darker mr-2 flex-shrink-0" blank
    elClass "div" "flex flex-col font-facit text-label h-full flex-grow" $ do
      elClass "div" "text-copy" $ text name
      elClass "div" "text-light" $ text email
