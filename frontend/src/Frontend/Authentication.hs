{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Authentication where

import Control.Monad.Fix
import Data.Vessel
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Vessel.AuthMapV
import Rhyolite.Vessel.ErrorV

import Common.Route
import Frontend.Utils

authenticateWithToken
  :: ( Ord token
     , EmptyView v
     , Semigroup (v Identity)
     , Eq (v (Const SelectedCount))
     , Group (v (Const SelectedCount))
     , Additive (v (Const SelectedCount))
     , MonadHold t m, MonadFix m
     , PostBuild t m, DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , Prerender js t m
     )
  => Dynamic t (Maybe token)
  -> RoutedT t r (QueryT t (v (Const SelectedCount)) (RequesterT t (ApiRequest () publicRequest privateRequest) Identity m)) (Event t a)
  -> RoutedT t r (RhyoliteWidget (AuthMapV token v (Const SelectedCount)) (ApiRequest token publicRequest privateRequest) t m) (Event t a)
authenticateWithToken mToken a = do
  pb <- getPostBuild
  fmap switchDyn $ widgetHold (pure never) $ ffor (leftmost [tag (current mToken) pb, updated mToken]) $ \case
    Nothing -> do
      r <- onRender
      setRoute $ FrontendRoute_Login :/ () <$ r
      pure never
    Just token -> do
      fmap switchDyn $ mapRoutedT (authenticatedWidget token) $ handleAuthFailure renderInvalid a

authenticatedWidget
  :: ( Ord token, View v
     , Group (v (Const SelectedCount)), Additive (v (Const SelectedCount)), Semigroup (v Identity)
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , MonadFix m, PostBuild t m
     )
  => token
  -> QueryT t (ErrorV () v (Const SelectedCount)) (RequesterT t (ApiRequest () publicRequest privateRequest) Identity m) a
  -> RhyoliteWidget (AuthMapV token v (Const SelectedCount)) (ApiRequest token publicRequest privateRequest) t m a
authenticatedWidget token = mapAuth token (authMapQueryMorphism token)

handleAuthFailure
  :: ( EmptyView v, PostBuild t m
     , Group (v (Const SelectedCount)), Additive (v (Const SelectedCount)), Semigroup (v Identity)
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , Adjustable t m, MonadHold t m, MonadFix m
     , Eq (v (Const SelectedCount))
     )
  => RoutedT t r (QueryT t () m) a
  -> RoutedT t r (QueryT t (v (Const SelectedCount)) m) a
  -> RoutedT t r (QueryT t (ErrorV () v (Const SelectedCount)) m) (Dynamic t a)
handleAuthFailure placeholder authenticatedChild = do
  pb <- getPostBuild
  errView <- eitherDyn . fmap observeErrorV =<< askQueryResult
  widgetHold (mapRoutedT (withQueryT unsafeProjectE) placeholder) $ ffor (leftmost [tag (current errView) pb, updated errView]) $ \case
    Left _ -> mapRoutedT (withQueryT unsafeProjectE) placeholder
    Right _ -> mapRoutedT (withQueryT unsafeProjectV) authenticatedChild

renderInvalid
  :: (DomBuilder t m)
  => m (Event t x)
renderInvalid = do
  el "p" $ text $ "Your token is invalid."
  pure never
