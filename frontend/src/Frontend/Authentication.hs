{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Authentication where

import Control.Monad.Fix
import Data.Vessel
import Data.Maybe
import Data.Semigroup.Commutative
import Reflex.Dom.Core
import Obelisk.Route
import Obelisk.Route.Frontend
import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Vessel.AuthMapV
import Rhyolite.Vessel.ErrorV
import Prelude hiding ((.))
import Control.Category

import Common.Route
import Frontend.Utils
import Rhyolite.Frontend.Auth.App
import Common.View
import Common.Schema
import Control.Monad.Ref
import qualified Data.Text as T
import Rhyolite.Vessel.AuthenticatedV
import qualified Data.Aeson as Ae
import Debug.Trace

{-
authenticateWithToken
  :: ( MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m, Prerender t m, Ref m ~ Ref (Performable m)
     , SetRoute t (R FrontendRoute) m
     , MonadQuery t (FullAppAuthErrorV MasterlistApp (Const SelectedCount)) m
     )
   {- ( Ord token
     , EmptyView v
     , Semigroup (v Identity)
     , Eq (v (Const SelectedCount))
     , Group (v (Const SelectedCount))
     , Commutative (v (Const SelectedCount))
     , MonadHold t m, MonadFix m
     , PostBuild t m, DomBuilder t m
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , Prerender t m
     ) -}
  => Dynamic t (Maybe AuthToken)
  -> RoutedT t r (AuthAppWidget MasterlistApp t m) (Event t a)
  -> RoutedT t r (FullAppWidget MasterlistApp t m) (Event t a)
authenticateWithToken mToken a = do
  pb <- getPostBuild
  fmap switchDyn $ widgetHold (pure never) $ ffor (leftmost [tag (current mToken) pb, updated mToken]) $ \case
    Nothing -> do
      r <- onRender
      setRoute $ FrontendRoute_Auth :/ AuthRoute_Login :/ () <$ r
      pure never
    Just token -> do
      fmap switchDyn $ mapRoutedT (authenticatedWidget (Proxy :: Proxy MasterlistApp) token) $ handleAuthFailure renderInvalid a
-}
{-authenticatedWidget
  :: ( Ord token, View v
     , Group (v (Const SelectedCount)), Commutative (v (Const SelectedCount)), Semigroup (v Identity)
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , MonadFix m, PostBuild t m
     )
  => token
  -> QueryT t (ErrorV () v (Const SelectedCount)) (RequesterT t (ApiRequest () publicRequest privateRequest) Identity m) a
  -> RhyoliteWidget (AuthMapV token v (Const SelectedCount)) (ApiRequest token publicRequest privateRequest) t m a
authenticatedWidget token = mapAuth token (authMapQueryMorphism token)
-}

handleAuthFailure
  :: ( PostBuild t m
     -- EmptyView v, PostBuild t m
     -- , Group (v (Const SelectedCount)), Commutative (v (Const SelectedCount)), Semigroup (v Identity)
     -- , QueryResult (v (Const SelectedCount)) ~ v Identity
     , Adjustable t m, MonadHold t m, MonadFix m
     , DomBuilder t m
     , Ref (Performable m) ~ Ref m
 --    , MonadQuery t (FullAppAuthErrorV MasterlistApp (Const SelectedCount)) m
     -- , Eq (v (Const SelectedCount))
     )
  => RoutedT t r (AuthErrorAppWidget MasterlistApp t m) a
  -> RoutedT t r (AuthAppWidget MasterlistApp t m) a
  -> RoutedT t r (AuthErrorAppWidget MasterlistApp t m) (Dynamic t a)
 -- -> RoutedT t r (QueryT t (v (Const SelectedCount)) m) a
 -- -> RoutedT t r (QueryT t (ErrorV () v (Const SelectedCount)) m) (Dynamic t a)
handleAuthFailure placeholder authenticatedChild = do
  pb <- getPostBuild
  errView <- eitherDyn . fmap (fromMaybe (Right mempty) . observeErrorV) =<< mapRoutedT (RhyoliteWidget . withQueryT disperseAuthenticatedErrorV) askQueryResult
  qr <- mapRoutedT (RhyoliteWidget . withQueryT disperseAuthenticatedErrorV) askQueryResult
  -- dynText $ (traceShowId . ("New qr: " <>) . T.pack . show . Ae.encode) <$> qr
  -- _ <- watch $ constDyn $ privateP ~> errorV ~> _ -- ~> key V_Hunts ~> key ()
  widgetHold placeholder $ traceEventWith (const "updating for auth") $ ffor (leftmost [tag (current errView) pb, updated errView]) $ \case
    Left (_) -> placeholder
    Right (_) -> mapRoutedT (RhyoliteWidget . withQueryT (disperseAuthenticatedErrorV . unsafeProjectV) . unRhyoliteWidget) authenticatedChild

unsafeProjectV :: (EmptyView v, QueryResult (v (Const SelectedCount)) ~ v Identity, Semigroup (v (Const SelectedCount))) => QueryMorphism (v (Const SelectedCount)) (ErrorV () v (Const SelectedCount))
unsafeProjectV = QueryMorphism { _queryMorphism_mapQuery = queryErrorVConst , _queryMorphism_mapQueryResult = mapQueryResult }
  where
    mapQueryResult r = case observeErrorV r of
      Nothing -> emptyV
      Just (Left _) -> emptyV
      Just (Right r') -> r'
    


renderInvalid
  :: (DomBuilder t m)
  => m (Event t x)
renderInvalid = do
  el "p" $ text $ "Your auth token is invalid, clearing."
  pure never
