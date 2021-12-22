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

import Prelude hiding (id, (.))
import Control.Category
import Data.Text (Text)
import Data.Functor.Identity
import Database.Id.Class
import Obelisk.Route
import Obelisk.Route.TH
import Rhyolite.Schema

import Common.Schema

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.
  BackendRoute_Listen :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_Signup :: FrontendRoute ()
  FrontendRoute_Channel :: FrontendRoute (Maybe (Id Chatroom))
  FrontendRoute_Templates :: FrontendRoute (R TemplateRoute)
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data TemplateRoute :: * -> * where
  -- The index route is where we select the template to view
  TemplateRoute_Index :: TemplateRoute ()
  -- The following routes should correspond to the non-template frontend routes
  -- (i.e., pages that users are expected to visit)
  TemplateRoute_Login :: TemplateRoute ()
  TemplateRoute_Signup :: TemplateRoute ()
  TemplateRoute_Channel :: TemplateRoute ()
  TemplateRoute_Main :: TemplateRoute ()

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
      FrontendRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_Channel -> PathSegment "channel" $ maybeEncoder (unitEncoder mempty) (singlePathSegmentEncoder . idEncoder)
      FrontendRoute_Templates -> PathSegment "templates" $ pathComponentEncoder $ \case 
        TemplateRoute_Index -> PathEnd $ unitEncoder mempty
        TemplateRoute_Login -> PathSegment "login" $ unitEncoder mempty
        TemplateRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
        TemplateRoute_Channel -> PathSegment "channel" $ unitEncoder mempty
        TemplateRoute_Main -> PathSegment "main" $ unitEncoder mempty)

checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error $ show e
  Right x -> x

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''TemplateRoute
  ]
