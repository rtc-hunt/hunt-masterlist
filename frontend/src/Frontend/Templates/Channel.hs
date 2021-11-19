{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Templates.Channel where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Data.Bool
import Data.Semigroup
import qualified Data.Text as T
import Data.Time
import Data.Vessel
import Data.Vessel.Map
import Data.Vessel.Vessel
import Database.Id.Class
import Obelisk.Route.Frontend
import Reflex
import Reflex.Dom.Core hiding (Request)
import Rhyolite.Api hiding (Request)
import Rhyolite.Frontend.App

import Common.Api
import Common.Route
import Common.Schema
import Common.View

import Frontend.Templates.Partials.Buttons
import Frontend.Templates.Partials.ChannelList
import Frontend.Templates.Partials.Headers
import Frontend.Templates.Partials.Switch

import Frontend.Utils

channel
  :: forall t m js.
     ( PostBuild t m
     , DomBuilder t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , MonadHold t m
     , MonadFix m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js t m
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     )
  => Dynamic t (Id Chatroom)
  -> m (Event t ())
channel cid = elClass "div" "w-screen h-screen bg-background flex flex-col" $ do
  header True
  elClass "div" "w-full flex flex-row flex-grow" $ do
    click <- elClass "div" "flex-shrink-0 w-1/4 h-full flex-col bg-sunken hidden md:flex border-r border-metaline" $
      channelList $ def & channelListConfig_useH2 .~ True
    channelInterior cid
    pure click

channelInterior
  :: forall t m js.
     ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     )
  => Dynamic t (Id Chatroom)
  -> m ()
channelInterior cid = elClass "div" "w-full flex flex-col" $ do
  mName <- watchView $ fmap (\c -> vessel V_Chatroom . mapVMorphism c) cid
  elClass "div" "p-4 bg-raised flex flex-row justify-between items-center border-b border-metaline relative" $ do
    elClass "div" "flex flex-col" $ do
      elClass "div" "font-karla font-bold text-h2 md:text-h1 text-copy leading-none" $ dynText $ ffor mName $ \case
        Nothing -> "Channel"
        Just (First name) -> name

    elClass "div" "flex flex-row items-center" $ do
      secondaryIconButton "" "search"
      elClass "div" "" $ do
        eClickMore <- secondaryIconButton "ml-2 md:hidden" "more_horiz"

        dMoreOpen <- foldDyn ($) False $ not <$ eClickMore
        let
          mkMenuClasses b =
            classList [ "absolute left-0 right-0 top-full p-4 md:hidden"
                      , "tranform transition-all duration-150"
                      , bool "pointer-events-none opacity-0" "pointer-events-auto opacity-100" b
                      ]

        elDynClass "div" (mkMenuClasses <$> dMoreOpen) $ do
          elClass "div" "bg-white rounded p-4 shadow-button z-10" $ do
            switchButton $ (def :: SwitchConfig t)
              & switchConfig_label .~ "Notifications"
              & switchConfig_icon .~ Just "notifications_none"
              & switchConfig_disabled .~ pure True

            elClass "div" "flex flex-row items-center mt-4" $ do
              elClass "div" "font-icon leading-none text-icon text-copy mr-1 " $ text "person_outline"
              elClass "div" "font-facit text-body text-copy" $ text "Members"

  mMessages <- (maybeDyn . fmap (completeMapOf =<<) =<<) $ watchView $ fmap (\c -> vessel V_Messages . mapVMorphism c) cid
  elClass "div" "flex-grow flex flex-col p-4" $ dyn $ ffor mMessages $ \case
    Nothing -> pure ()
    Just ms -> do
      -- Stubbing out the design which distinguishes between the current user's messages and
      -- messages from other people.
      listWithKey ms (message (MessageConfig { _messageConfig_viewer = pure True }))
      pure ()

  elClass "div" "p-1 bg-white flex flex-row justify-center border-t border-metaline" $ do
    secondaryIconButton "" "add"
    rec inputBox <- inputElement $ def
          & initialAttributes .~ ( "class" =: "focus:outline-none mx-1 font-facit font-label text-label placeholder-light px-3.5 bg-inset rounded shadow-input flex-grow"
                                   <> "placeholder" =: "Type your message"
                                   <> "type" =: "text"
                                 )
          & inputElementConfig_setValue .~ ("" <$ sendMessage)
        sendClick <- iconButton "send"
        let sendEnter = fmapMaybe (\w -> guard (w==13) >> pure ()) $ domEvent Keypress (_inputElement_element inputBox)
            sendMessage = leftmost [sendClick, sendEnter]
    let newMessage = value inputBox
    _ <- fmap fanEither . requestingIdentity . ffor (tag (current ((,) <$> cid <*> newMessage)) sendMessage) $ \(c, payload) ->
      ApiRequest_Private () $ PrivateRequest_SendMessage c payload
    pure ()

messageFullWidth :: DomBuilder t m => m ()
messageFullWidth = do
  elClass "div" "font-facit text-copy flex flex-col mt-4" $ do
    elClass "div" "flex flex-row items-baseline justify-between" $ do
      elClass "div" "text-label" $ text "Tanko"
      elClass "div" "font-bold text-label text-light" $ text "2:00am"
    elClass "div" "p-4 rounded border border-metaline bg-white" $ text "What is good?"

data MessageConfig t = MessageConfig
   { _messageConfig_viewer :: Dynamic t Bool
   }

message
  :: (PostBuild t m, DomBuilder t m)
  => MessageConfig t
  -> (UTCTime, Id Message)
  -> Dynamic t MsgView
  -> m ()
message (MessageConfig dViewer) (sendTime, _) mView = do
  elDynClass "div" (mkClasses <$> dViewer) $ do
    elClass "div" "flex flex-row items-baseline justify-between" $ do
      elClass "div" "text-label md:mr-4" $ dynText (fmap _msgView_handle mView)
      elClass "div" "font-bold text-label text-light" $ text $ T.pack $
        formatTime defaultTimeLocale "%R" sendTime
    elClass "div" "p-4 rounded border border-metaline bg-white w-auto" $ dynText (fmap _msgView_text mView)

  where
    mkClasses b =
      classList [ "font-facit text-copy flex flex-col mb-6"
                , bool "mr-16 md:mr-0 md:items-start" "ml-16 md:ml-0 md:items-end" b
                ]

