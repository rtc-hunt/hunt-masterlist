{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Frontend.Puzzle where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (ViewR(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import Data.Semigroup (First(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vessel
import qualified GHCJS.DOM.Element as JS
import GHCJS.DOM.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (El)
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Frontend.App
import Rhyolite.Vessel.Path
import Data.Map.Monoidal as MMap hiding (keys)
import Rhyolite.SemiMap

import Templates (ChannelConfig(..), ChannelOut(..), MessagesConfig(..))
import qualified Templates as Templates
import Templates.Partials.ChannelList
import Templates.Partials.Message as Templates
import Templates.Types

import Common.Request
import Common.Route
import Common.Schema
import Common.View
import Database.Beam.Schema

import Frontend.ChannelList
import Frontend.Channel
import Frontend.Types
import Frontend.Utils

import Control.Monad.Fix

import Reflex.Dom.Core
import Obelisk.Route.Frontend

import Templates.Frame
import Templates.Puzzle as Template
import Templates.PuzzleList as Template
import Common.Schema
import Debug.Trace

puzzles :: (Monad m, MonadFix m, Reflex t, Routed t (Maybe (Id Puzzle)) m, Adjustable t m, NotReady t m, PostBuild t m, DomBuilder t m, MonadHold t m
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     , SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , MonadQuery t (Vessel V (Const SelectedCount)) (Client m)
     , Prerender js t m
     , Requester t (Client m)
     , SetRoute t (R FrontendRoute) (Client m)
     , MonadIO (Performable m)
     , Response (Client m) ~ Identity
     , Request (Client m) ~ ApiRequest () PublicRequest PrivateRequest
  ) => m (Event t ())
puzzles = do
  mPuzzle <- askRoute
  dyn $ ffor mPuzzle $ \case
    Nothing -> masterlist
    Just i -> puzzle i
  return never

data MasterlistPage
  = MasterlistPage_List
  | MasterlistPage_HuntPage
  | MasterlistPage_Chat
  deriving (Enum, Ord, Eq)

masterlistPageToText :: MasterlistPage -> Text
masterlistPageToText = \case
  MasterlistPage_List -> "List"
  MasterlistPage_HuntPage -> "Hunt Frontpage"
  MasterlistPage_Chat -> "Chat"

masterlist :: (Monad m, MonadHold t m, PostBuild t m, Reflex t, DomBuilder t m, MonadFix m
     , SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, Prerender js t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , SetRoute t (R FrontendRoute) (Client m)
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , MonadQuery t (Vessel V (Const SelectedCount)) (Client m)
     , Response (Client m) ~ Identity
     , Request (Client m) ~ ApiRequest () PublicRequest PrivateRequest
     , Requester t (Client m)
     , Prerender js t m
     , MonadIO (Performable m)
  )
  => m ()
masterlist = do
  hunt <- buildHunt
  framed $ Framed
    { _framed_headerItems = mdo
            let tabs = [MasterlistPage_List .. MasterlistPage_Chat]
            evts <- fmap (zipWith (<$) tabs) $ 
             sequence $ ffor tabs $ \tab ->
              let itemClass = ffor activeTab $ \aTab ->
                     "class" =: if aTab == tab then "item active" else "item"
              in fmap (domEvent Click . fst) $ elDynAttr' "div" itemClass $ text $ masterlistPageToText tab
            activeTab :: Dynamic t MasterlistPage <- holdDyn MasterlistPage_List $ leftmost $ zipWith (<$) [MasterlistPage_List, MasterlistPage_HuntPage, MasterlistPage_Chat] evts
            -- activeSolverList puzId puzzlesData
            return $ activeTab
    , _framed_body = \(activeTab :: Dynamic t MasterlistPage) cmdString -> do
        let frameURI uriD = divClass "framed" $ elDynAttr "iframe" (("src" =: ) <$> uriD) blank
        myTabDisplay "ui top attached tabular menu" "activeTab" activeTab $
          MasterlistPage_List =: ("List", do
            puzzleListD <- puzzleListBuilder
            puzzlesTable PuzzleTableConfig
              { _puzzleTableConfig_results = puzzleListD
              , _puzzleTableConfig_puzzleLink = \id -> routeLink $ FrontendRoute_Puzzle :/ Just id
              })
           <> MasterlistPage_HuntPage =: ("frontpage", do
               frameURI $ _hunt_rootpage <$> hunt
              )
           <> MasterlistPage_Chat =: ("Chat",
                      let mcid = (Just) <$> (_hunt_channel <$> hunt)
                          mkMsgReq c m = case c of
                            Nothing -> Nothing
                            Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMessage c' m
                      in do
                       channelView <- channelBuilder mcid
                       void $ prerender blank $ do
                        rec (msgs, _) <- elAttr' "div" ("class" =: "ui container p-4 flex-grow flex flex-col overflow-y-scroll" <> "style" =: "height: 100%") $ do
                              dyn_ $ ffor (_channelView_messages channelView) $ \case
                                Nothing -> text "No messages"
                                Just ms -> messagesHelper msgs ms
                        _ <- requestingIdentity $ attachWithMaybe (\c m -> mkMsgReq c m) (current mcid) cmdString
                        blank
                 )
    }

buildHunt
  :: ( Reflex t
     , Monad m
     )
  => m (Dynamic t (Hunt Identity))
buildHunt = pure $ constDyn $ Hunt { _hunt_id = 1, _hunt_title = "Test Hunt", _hunt_rootpage = "http://hackaday.com", _hunt_channel = ChatroomId 1 }
  

puzzlePageTabs :: [PuzzlePageTab]
puzzlePageTabs = [minBound .. maxBound]

tabToText :: PuzzlePageTab -> Text
tabToText = \case
  PuzzlePageTab_Sheet -> "Sheet"
  PuzzlePageTab_Puzzle -> "Puzzle"
--  PuzzlePageTab_Tools -> "Tools"
  PuzzlePageTab_Chat -> "Chat"
  PuzzlePageTab_Config -> "Puzzle Config"

puzzle :: (Monad m, Reflex t, DomBuilder t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     , SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, Prerender js t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , SetRoute t (R FrontendRoute) (Client m)
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , MonadQuery t (Vessel V (Const SelectedCount)) (Client m)
     , Response (Client m) ~ Identity
     , Request (Client m) ~ ApiRequest () PublicRequest PrivateRequest
     , Requester t (Client m)
     , Prerender js t m
     , MonadIO (Performable m)
  ) => Id Puzzle -> m ()
puzzle puz = do
  puzzleDataDM <- (puzzleBuilder $ constDyn puz) >>= maybeDyn
  knownTags <- getKnownTags
  dyn_ $ ffor puzzleDataDM $ \case
    Nothing -> blank
    Just puzzleData -> framed $ Framed
        {
          _framed_headerItems = mdo
            evts <- fmap (zipWith (<$) puzzlePageTabs) $ 
             sequence $ ffor puzzlePageTabs $ \tab ->
              let itemClass = ffor activeTab $ \aTab ->
                     "class" =: if aTab == tab then "item active" else "item"
              in fmap (domEvent Click . fst) $ elDynAttr' "div" itemClass $ text $ tabToText tab
            activeTab <- holdDyn (PuzzlePageTab_Sheet) $ leftmost evts
            -- activeSolverList puzId puzzlesData
            return $ activeTab
        , _framed_body = \ activeTab cmd -> do
            puzzleView PuzzleConfig
              { _puzzleConfig_puzzle = puzzleData
              , _puzzleConfig_tab = activeTab
              , _puzzleConfig_chatWidget =
                      let mcid = (fmap ChatroomId . unChatroomId . _puzzle_Channel) <$> (puzzleData >>= _puzzleData_puzzle)
                          mkMsgReq c m = case c of
                            Nothing -> Nothing
                            Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMessage c' m
                      in do
                       channelView <- channelBuilder mcid
                       void $ prerender blank $ do
                        rec (msgs, _) <- elAttr' "div" ("class" =: "ui container p-4 flex-grow flex flex-col overflow-y-scroll" <> "style" =: "height: 100%") $ do
                              dyn_ $ ffor (_channelView_messages channelView) $ \case
                                Nothing -> text "No messages"
                                Just ms -> messagesHelper msgs ms
                        _ <- requestingIdentity $ attachWithMaybe (\c m -> mkMsgReq c m) (current mcid) cmd
                        blank
              , _puzzleConfig_configuratorWidget = do
                  cfgOut <- puzzleConfigurator PuzzleConfiguratorConfig
                    { _puzzleConfiguratorConfig_puzzle = puzzleData
                    , _puzzleConfiguratorConfig_knownTags = knownTags
                    }
                  _ <- requestingIdentity $ ApiRequest_Private () . PrivateRequest_UpdatePuzzle <$> _puzzleConfiguratorOut_puzzle cfgOut
                  _ <- requestingIdentity $ ApiRequest_Private () . PrivateRequest_PuzzleCommand . uncurry (PuzzleCommand_Solve puz) <$> _puzzleConfiguratorOut_addSolution cfgOut
                  _ <- requestingIdentity $ ApiRequest_Private () . PrivateRequest_PuzzleCommand . PuzzleCommand_UnSolve <$> _puzzleConfiguratorOut_removeSolution cfgOut
                  _ <- requestingIdentity $ ApiRequest_Private () . PrivateRequest_PuzzleCommand . PuzzleCommand_AddMeta puz <$> _puzzleConfiguratorOut_addMeta cfgOut
                  _ <- requestingIdentity $ ApiRequest_Private () . PrivateRequest_PuzzleCommand . PuzzleCommand_RemoveMeta <$> _puzzleConfiguratorOut_removeMeta cfgOut
                  _ <- requestingIdentity $ ApiRequest_Private () . PrivateRequest_PuzzleCommand . PuzzleCommand_Tag puz <$> _puzzleConfiguratorOut_addTag cfgOut
                  _ <- requestingIdentity $ ApiRequest_Private () . PrivateRequest_PuzzleCommand . PuzzleCommand_Untag puz <$> _puzzleConfiguratorOut_removeTag cfgOut
                  _ <- requestingIdentity $ ApiRequest_Private () . PrivateRequest_PuzzleCommand . PuzzleCommand_Note puz <$> _puzzleConfiguratorOut_addNote cfgOut
                  blank
              }
        }

getKnownTags
  :: forall t m js.
     ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     )
  => m (Dynamic t (Set Text))
getKnownTags = do
  tags <- watch $ constDyn $ key V_UniqueTags ~> key () ~> postMap (traverse (fmap (Map.keysSet . getMonoidalMap) . getComplete))
  return $ (<> ["solved", "in-progress", "stalled", "extraction", "done"]) . fromMaybe mempty <$> tags
  

puzzleBuilder
  :: forall t m js.
     ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     )
  => Dynamic t (Id Puzzle)
  -> m (Dynamic t (Maybe (PuzzleData t)))
puzzleBuilder puzIdD = do
  puzzle <- fmap (fmap (fmap getFirst)) $ watch $ (\puz -> key V_Puzzle ~> key puz) <$> puzIdD
  metas <- watch $ (\puzId -> key V_Metas ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD

  metas_puzzles <- fmap (fmap (fmap (fmap getFirst))) $ watch $ (\puz -> key V_Puzzle ~> keys (Map.keysSet puz)) . Map.mapKeys _meta_Metapuzzle . fromMaybe mempty <$> metas
  let metaIdTitle = fmap (fmap _puzzle_Title) $ fromMaybe mempty <$> metas_puzzles
  tags <- watch $ (\puzId -> key V_Tags ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  solutions <- watch $ (\puzId -> key V_Solutions ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  notes <- watch $ (\puzId -> key V_Notes ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  -- display puzzles
  return $ ffor puzzle $ \foundPuzD ->
    ffor foundPuzD $ \puz -> PuzzleData
    { _puzzleData_puzzle = constDyn puz
    , _puzzleData_metas = metaIdTitle
    , _puzzleData_tags = (() <$) . Map.mapKeys _tagId_Tag <$> fromMaybe mempty <$> tags
    , _puzzleData_solutions = fromMaybe mempty <$> solutions
    , _puzzleData_notes = fromMaybe mempty <$> notes
    , _puzzleData_status = constDyn ""
    , _puzzleData_currentSolvers = constDyn mempty
    }

puzzleListBuilder
  :: forall t m js.
     ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     , MonadQuery t (Vessel V (Const SelectedCount)) m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Requester t m, Response m ~ Identity, Request m ~ ApiRequest () PublicRequest PrivateRequest
     )
  => m (Dynamic t (Map (Id Puzzle) (PuzzleData t)))
puzzleListBuilder = do
  puzzleIds <- watch $ pure $ key V_HuntPuzzles ~> key (HuntId 1 :: Id Hunt) ~> postMap (traverse (fmap getMonoidalMap . getComplete))
  -- This is a hack. We should be querying it all in batches and rebuilding a map.
  -- I'm lazy.
  rv <- listWithKey (fromMaybe mempty <$> puzzleIds) (\k _ -> puzzleBuilder $ constDyn k)
  return $ fmapMaybe id <$> joinDynThroughMap rv

  {-
  
  puzzles <- watch $ (\puzs -> key V_Puzzle ~> keys puzs ~> postMap (pure . fmap (fmap getFirst))) . Map.keysSet . fromMaybe mempty <$> puzzleIds
  
  metas :: Dynamic t (Map (Id Puzzle) (SemiMap (Metapuzzle Identity) ())) <- fmap (fmap (fromMaybe mempty)) $ watch $ (\puzIds -> key V_Metas ~> keys puzIds) . Map.keysSet . (fromMaybe mempty) <$> puzzleIds

  -- Crazy stuff to join Puzzle, Meta, Puzzle on the frontend
  let metaSomething :: Dynamic t (Map (Id Puzzle) (Map (Id Puzzle) ())) = fmap (Map.mapKeys _meta_Metapuzzle . fromMaybe mempty . fmap getMonoidalMap . getComplete) <$> metas
  let metaIdTitle = fmap . Map.intersection . fromMaybe mempty <$> puzzles <*> metaSomething

  --metas_puzzles <- fmap (fmap (fmap (fmap getFirst))) $ watch $ (\puz -> key V_Puzzle ~> keys (Map.keysSet puz)) . Map.mapKeys _meta_Metapuzzle . fromMaybe mempty <$> metas
  --let metaIdTitle = fmap (fmap _puzzle_Title) $ fromMaybe mempty <$> metas_puzzles
  --tags <- watch $ (\puzId -> key V_Tags ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  --solutions <- watch $ (\puzId -> key V_Solutions ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  --notes <- watch $ (\puzId -> key V_Notes ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  

  {- metas <- watch $ (\puzId -> key V_Metas ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  let metaIdTitle = Map.intersection <$> (fromMaybe mempty <$> puzzles) <*> (Map.mapKeys _meta_Metapuzzle . fromMaybe mempty <$> metas)
  tags <- watch $ (\puzId -> key V_Tags ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  solutions <- watch $ (\puzId -> key V_Solutions ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  notes <- watch $ (\puzId -> key V_Notes ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  -- display puzzles
  -}
  return $ ffor puzzles $ \puzzles -> ffor (fromMaybe mempty puzzles) $ \puz ->
    PuzzleData
      { _puzzleData_puzzle = constDyn puz
      , _puzzleData_metas = metaIdTitle -- fmap _puzzle_Title <$> metaIdTitle
      , _puzzleData_tags = constDyn mempty -- (() <$) . Map.mapKeys _tagId_Tag <$> fromJust <$> tags
      , _puzzleData_solutions = constDyn mempty -- fromJust <$> solutions
      , _puzzleData_notes = constDyn mempty -- fromJust <$> notes
      , _puzzleData_status = constDyn ""
      , _puzzleData_currentSolvers = constDyn mempty
      }
  {-
  return $ ffor (Map.lookup <$> puzIdD <*> (fromMaybe mempty <$> puzzles)) $ \foundPuzD ->
    ffor foundPuzD $ \puz -> PuzzleData
    { _puzzleData_puzzle = constDyn puz
    , _puzzleData_metas = fmap _puzzle_Title <$> metaIdTitle
    , _puzzleData_tags = (() <$) . Map.mapKeys _tagId_Tag <$> fromMaybe mempty <$> tags
    , _puzzleData_solutions = fromMaybe mempty <$> solutions
    , _puzzleData_notes = fromMaybe mempty <$> notes
    , _puzzleData_status = constDyn ""
    , _puzzleData_currentSolvers = constDyn mempty
    } -}
{-
  return $ constDyn $ Just $ PuzzleData
    { _puzzleData_puzzle = (Map.lookup) <$> (fromMaybe mempty <$> puzzles) <*> puzIdD
    , _puzzleData_metas = constDyn mempty -- fmap _puzzle_Title <$> metaIdTitle
    , _puzzleData_tags = constDyn mempty -- (() <$) . Map.mapKeys _tagId_Tag <$> fromJust <$> tags
    , _puzzleData_solutions = constDyn mempty -- fromJust <$> solutions
    , _puzzleData_notes = constDyn mempty -- fromJust <$> notes
    , _puzzleData_status = constDyn ""
    , _puzzleData_currentSolvers = constDyn mempty
    }
-}
 -}