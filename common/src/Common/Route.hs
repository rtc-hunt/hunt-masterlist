{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.
  BackendRoute_Listen :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_SignUp :: FrontendRoute ()

  -- TODO(skylar): These are temp to show off static stuff potentially
  FrontendRoute_Widgets :: FrontendRoute ()
  FrontendRoute_Channels :: FrontendRoute ()
  FrontendRoute_Channel :: FrontendRoute ()
  FrontendRoute_ChannelMembers :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_SignUp -> PathSegment "signup" $ unitEncoder mempty
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_Widgets -> PathSegment "widgets" $ unitEncoder mempty
      FrontendRoute_Channels -> PathSegment "channels" $ unitEncoder mempty
      FrontendRoute_Channel -> PathSegment "channel" $ unitEncoder mempty
      FrontendRoute_ChannelMembers -> PathSegment "members" $ unitEncoder mempty
      FrontendRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
  )

checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error $ show e
  Right x -> x

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
