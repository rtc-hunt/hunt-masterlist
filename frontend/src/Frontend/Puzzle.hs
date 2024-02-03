{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.Puzzle where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Lens
import Data.Semigroup (Endo(..))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Either
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
import Rhyolite.Vessel.Path as Path
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

import GHCJS.DOM
import GHCJS.DOM.Document

import Frontend.Chat
import Frontend.ChannelList
import Frontend.Channel
import Frontend.Cli
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

puzzles :: (Monad m, MonadFix m, Reflex t, Routed t (Id Hunt, Either PuzzleQuery (Id Puzzle)) m, Adjustable t m, NotReady t m, PostBuild t m, DomBuilder t m, MonadHold t m
     , Routed t ((Id Hunt, Either PuzzleQuery (Id Puzzle))) (Client m)
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
     , MonadJSM (Performable (Client m))
     , Response (Client m) ~ Identity
     , Request (Client m) ~ ApiRequest () PublicRequest PrivateRequest
     , MountableDomBuilder t m
  ) => m (Event t ())
puzzles = do
  routeD <- join <$> prerender (pure $ constDyn (HuntId 1, Left mempty)) askRoute
  isMainList <- holdUniqDyn $ fmap (Data.Either.fromRight Nothing . fmap Just) <$> routeD
  dyn $ ffor isMainList $ \case
    (hunt, Nothing) -> masterlist hunt $ Data.Either.fromLeft mempty . snd <$> routeD
    (_, Just i) -> puzzle i
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
     , MountableDomBuilder t m
  )
  => Id Hunt -> Dynamic t PuzzleQuery -> m ()
masterlist huntId queryD = do
  hunt <- buildHunt huntId
  knownMetas <- fmap (fmap (fromMaybe mempty)) $ watch $ constDyn $ key V_HuntMetas ~> key huntId ~> postMap (traverse (fmap getMonoidalMap . getComplete))
  knownTags <- getKnownTags
  manageDocumentTitle $ ("Hunt Master List " <>) . review puzzleQueryStringOrPuzzle_prism . Left <$> queryD
  framed $ Framed
    { _framed_hunt = constDyn huntId
    , _framed_headerItems = mdo
            let tabs = [MasterlistPage_List .. MasterlistPage_Chat]
            evts <- fmap (zipWith (<$) tabs) $ 
             sequence $ ffor tabs $ \tab ->
              let itemClass = ffor activeTab $ \aTab ->
                     "class" =: if aTab == tab then "item active" else "item"
              in fmap (domEvent Click . fst) $ elDynAttr' "div" itemClass $ text $ masterlistPageToText tab
            activeTab :: Dynamic t MasterlistPage <- holdDyn MasterlistPage_List $ leftmost $ zipWith (<$) [MasterlistPage_List, MasterlistPage_HuntPage, MasterlistPage_Chat] evts
            -- activeSolverList puzId puzzlesData
            return $ activeTab
    , _framed_body = \(activeTab :: Dynamic t MasterlistPage) msgString cmdString _ -> do
        let frameURI uriD = divClass "framed" $ elDynAttr "iframe" (("src" =: ) <$> uriD) blank
        let mcid = (Just) <$> (_hunt_channel <$> hunt)
            chatWidget cls = do
                   channelView <- channelBuilder mcid
                   void $ prerender blank $ do
                    rec (msgs, _) <- elAttr' "div" ("class" =: cls <> "style" =: "height: 100%; flex-direction: column-reverse; display: flex;") $ divClass "flex-grow flex flex-col" $ do
                          dyn_ $ ffor (_channelView_messages channelView) $ \case
                            Nothing -> text "No messages"
                            Just ms -> void $ listWithKey ms $ \_ -> Templates.message
                    blank
            mkMsgReq c m = case c of
                        Nothing -> Nothing
                        Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMessage c' m
        chatOverlay $ mcid
        _ <- requestingIdentity $ attachWithMaybe (\c m -> mkMsgReq c m) (current mcid) msgString
        
        myTabDisplay "ui top attached tabular menu" "activeTab" activeTab $
          MasterlistPage_List =: ("List", do
            puzzleListD <- puzzleListBuilder huntId
            puzzlesTable PuzzleTableConfig
              { _puzzleTableConfig_query = queryD
              , _puzzleTableConfig_modifyQuery = \e -> do
                   modifyRoute $ (\(Endo mod) -> \case
                      FrontendRoute_Puzzle :/ (hunt, currentQuery) -> FrontendRoute_Puzzle :/ (hunt, over _Left mod $ currentQuery)
                      a -> a) <$> e
              , _puzzleTableConfig_results = puzzleListD
              , _puzzleTableConfig_puzzleLink = \id -> dynRouteLink $ (\i -> FrontendRoute_Puzzle :/ (huntId, Right i)) <$> id
              , _puzzleTableConfig_metas = knownMetas
              , _puzzleTableConfig_tags = knownTags
              }
              )
           <> MasterlistPage_HuntPage =: ("frontpage", do
               frameURI $ _hunt_rootpage <$> hunt
              )
           <> MasterlistPage_Chat =: ("Chat",
                      chatWidget "ui container p-4 flex-grow flex flex-col overflow-y-scroll"
                 )
        let showAddPuzzle = \case
              MasterlistPage_List -> "class" =: "bottomwidget"
              _ -> "class" =: "hidden"
        elDynAttr "div" (showAddPuzzle <$> activeTab) $ do
              elClass "form" "ui form" $ divClass "inline fields" $ prerender_ (return ()) $ mdo
                let labeledField label = divClass "field" $ do
                      el "label" $ text label
                      inputElement $ def & inputElementConfig_setValue .~ ("" <$ reqDone)
                title <- labeledField "Title"
                isMeta <- divClass "field" $ divClass "ui toggle checkbox" $ do
                  ie <- inputElement $ def
                           & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox")
                           & inputElementConfig_setChecked .~ (False <$ reqDone)
                  el "label" $ text "Is meta?"
                  return ie
                url <- labeledField "URL" 
  
                let curNewPuzzle = current $ PrivateRequest_AddPuzzle <$> _inputElement_value title <*> _inputElement_checked isMeta <*> _inputElement_value url <*> pure huntId
                
                pzl <- buttonOneshotClass "ui button" "Add Puzzle" $ () <$ reqDone
                reqDone <- requestingIdentity $ ApiRequest_Private () <$> curNewPuzzle <@ pzl
                blank
        divClass "chat-sidebar" $ chatWidget "flex flex-col flex-grow p-4 overflow-y-scroll"
        let (cliErrors, cmdSel) = parseCli Nothing cmdString
        clearErrors <- fmap switchDyn $ prerender (return never) $ debounce 10 $ "" <$ cliErrors
        lastError <- holdDyn "" $ leftmost [cliErrors, clearErrors]
        requestingSimpleCommands cmdSel
        _ <- requestingIdentity $ attachWithMaybe (\c a -> case c of { Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMe c' a; Nothing -> Nothing; }) (current mcid) $ select cmdSel CliCommandTag_Me -- attachWithMaybe (\c m -> mkMsgReq c m) (current mcid) msgString
        elClass "pre" "commandOutput" $ dynText lastError

          
    , _framed_layout = \ (MenuSettings layout) tab -> (\t l -> if t == MasterlistPage_Chat then MutedChat else l) <$> tab <*> layout
    }

huntselect :: (Monad m, MonadHold t m, PostBuild t m, Reflex t, DomBuilder t m, MonadFix m
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
  => m (Event t ())
huntselect = do
  elClass "nav" "app ui fixed inverted menu" $ mdo
    routeLink (FrontendRoute_HuntSelection :/ ()) $ divClass "logo header item whitespace-nowrap" $ text "Hunt Master List"

  hunts <- buildHunts
  divClass "appMain" $ el "div" $ el "div" $ elClass "ul" "grid h-screen place-items-center" $ el "div" $ do
    elClass "div" "huntlist-title" $ text "Select a hunt"
    list hunts $ \dHunt ->
      el "li" $ dynRouteLink ((\a -> FrontendRoute_Puzzle :/ (HuntId $ _hunt_id a, Left mempty)) <$> dHunt) $ elClass "div" "text-lg huntlist-button" $ dynText $ _hunt_title <$> dHunt
  pure never



buildHunts
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
--  :: ( Reflex t
--     , Monad m
--     )
  =>
  m (Dynamic t (Map (Id Hunt) (Hunt Identity)))
buildHunts = do
  dynHuntMaybe <- watch $ constDyn $ key V_Hunts ~> key () ~> postMap (traverse (fmap getMonoidalMap . getComplete))
  pure $ fromMaybe mempty <$> dynHuntMaybe

buildHunt
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
--  :: ( Reflex t
--     , Monad m
--     )
  =>
  Id Hunt -> m (Dynamic t (Hunt Identity))
buildHunt huntId = do 
  dynHuntMaybe <- watch $ constDyn $ key V_Hunts ~> key () ~> postMap (traverse (fmap getMonoidalMap . getComplete))
  let terriblePlaceholderHunt = Hunt { _hunt_id = 1, _hunt_title = "Test Hunt", _hunt_rootpage = "https://www.starrats.org/", _hunt_channel = ChatroomId 1 }
  return $ fromMaybe terriblePlaceholderHunt . ((Map.!? huntId) =<<) <$> dynHuntMaybe
  -- return $ (<> statusTags) . fromMaybe mempty <$> tags
  -- hunts <- watch $ pure $ V_Hunts ~> postMap (traverse (fmap getMonoidalMap . getComplete))
  -- pure $ constDyn $ Hunt { _hunt_id = 1, _hunt_title = "Test Hunt", _hunt_rootpage = "https://www.starrats.org/", _hunt_channel = ChatroomId 1 }
  

puzzlePageTabs :: [PuzzlePageTab]
puzzlePageTabs = [minBound .. maxBound]

tabToText :: PuzzlePageTab -> Text
tabToText = \case
  PuzzlePageTab_Sheet -> "Sheet"
  PuzzlePageTab_Puzzle -> "Puzzle"
--  PuzzlePageTab_Tools -> "Tools"
  PuzzlePageTab_Chat -> "Chat"
  PuzzlePageTab_Config -> "Puzzle Config"

manageDocumentTitle
   :: ( MonadJSM (Performable (Client m))
      , Prerender js t m
      , PerformEvent t m
      , Reflex t
      , PostBuild t m
      )
   => Dynamic t Text
   -> m ()
manageDocumentTitle titleD = do
      pb <- getPostBuild
      prerender_ blank $ performEvent_ $ ffor (leftmost [updated titleD, current titleD <@ pb ]) $ \title -> do
        doc <- currentDocumentUnchecked
        setTitle doc title

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
     , MonadJSM (Performable (Client m))
     , Prerender js t m
     , MonadIO (Performable m)
  ) => Id Puzzle -> m ()
puzzle puz = do
  puzzleDataDM <- (puzzleBuilder $ constDyn puz) >>= maybeDyn
  knownTags <- getKnownTags
  -- knownMetas <- fmap (fmap (fromMaybe mempty)) $ watch $ constDyn $ key V_HuntMetas ~> key (HuntId 1 :: Id Hunt) ~> postMap (traverse (fmap getMonoidalMap . getComplete))
  dyn_ $ ffor puzzleDataDM $ \case
    Nothing -> blank
    Just puzzleData -> do
      let huntId = _puzzle_Hunt <$> (puzzleData >>= _puzzleData_puzzle)
      knownMetas <- fmap (fmap (fromMaybe mempty)) $ watch $ (\hunt -> key V_HuntMetas ~> key hunt ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> huntId
      manageDocumentTitle $ ("H.M.L.: " <>) . _puzzle_Title <$> (puzzleData >>= _puzzleData_puzzle)
      framed $ Framed
        { _framed_hunt = huntId
        , _framed_headerItems = mdo
            evts <- fmap (zipWith (<$) puzzlePageTabs) $ 
             sequence $ ffor puzzlePageTabs $ \tab ->
              let itemClass = ffor activeTab $ \aTab ->
                     "class" =: if aTab == tab then "item active" else "item"
              in fmap (domEvent Click . fst) $ case tab of
                    PuzzlePageTab_Puzzle -> elDynAttr' "div" itemClass $ do
                        text $ tabToText tab
                        let lnk = _puzzle_URI <$> (puzzleData >>= _puzzleData_puzzle)
                            lattr = ((\l -> ("target":: Text) =: "_blank" <> "href" =: l) <$> lnk)
                        elDynAttr "a" lattr $ elClass "i" "external alternate icon px-4" $ blank
                    PuzzlePageTab_Sheet -> elDynAttr' "div" itemClass $ do
                        text $ tabToText tab
                        let lnk = _puzzle_SheetURI <$> (puzzleData >>= _puzzleData_puzzle)
                            lattr = ((\l -> ("target":: Text) =: "_blank" <> "href" =: (fromMaybe "" l)) <$> lnk)
                        elDynAttr "a" lattr $ elClass "i" "external alternate icon px-4" $ blank
                    tab -> elDynAttr' "div" itemClass $ text $ tabToText tab
            activeTab <- holdDyn (PuzzlePageTab_Sheet) $ leftmost evts
            divClass "item menuShrink" $ divClass "ellipsisShrink" $ do
              text "Title: "
              dynText $ _puzzle_Title <$> (puzzleData >>= _puzzleData_puzzle)
            activeSolverList $ puzzleData >>= _puzzleData_currentSolvers
            let lnkD = _puzzle_voicelink <$> (puzzleData >>= _puzzleData_puzzle)
             in elDynAttr "a" (fromMaybe mempty . fmap ((<> ("class" =: "item" <> "target" =: "_blank")) . ("href" =:)) <$> lnkD) $ (dynText $ fromMaybe "" . (fmap (const "Voice Chat")) <$> lnkD)
            return $ activeTab
        , _framed_body = \ activeTab newMsg cmd _ -> do -- divClass "" $ do
            chatOverlay $ fmap ChatroomId . unChatroomId . _puzzle_Channel <$> (puzzleData >>= _puzzleData_puzzle) -- divClass "chat-overlay scrollable" $ text "Chat Messagez"
            let mcid = (fmap ChatroomId . unChatroomId . _puzzle_Channel) <$> (puzzleData >>= _puzzleData_puzzle)
                chatWidget cls = do
                       channelView <- channelBuilder mcid
                       void $ prerender blank $ do
                        rec (msgs, _) <- elAttr' "div" ("class" =: cls <> "style" =: "height: 100%; flex-direction: column-reverse; display: flex;") $ divClass "flex-grow flex flex-col" $ do
                              dyn_ $ ffor (_channelView_messages channelView) $ \case
                                Nothing -> text "No messages"
                                Just ms -> void $ listWithKey ms $ \_ -> Templates.message
                        blank
                mkMsgReq c m = case c of
                            Nothing -> Nothing
                            Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMessage c' m
            _ <- requestingIdentity $ attachWithMaybe (\c m -> mkMsgReq c m) (current mcid) newMsg
            
            let (cliErrors, cmdSel) = parseCli (Just puz) cmd
            clearErrors <- fmap switchDyn $ prerender (return never) $ debounce 10 $ "" <$ cliErrors
            lastError <- holdDyn "" $ leftmost [cliErrors, clearErrors]
            requestingSimpleCommands cmdSel
            _ <- requestingIdentity $ attachWithMaybe (\c a -> case c of { Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMe c' a; Nothing -> Nothing; }) (current mcid) $ select cmdSel CliCommandTag_Me -- attachWithMaybe (\c m -> mkMsgReq c m) (current mcid) msgString


            puzzleView PuzzleConfig
              { _puzzleConfig_puzzle = puzzleData
              , _puzzleConfig_tab = activeTab
              , _puzzleConfig_chatWidget = chatWidget "ui container p-4 flex-grow flex flex-col overflow-y-scroll"
              , _puzzleConfig_configuratorWidget = do
                  cfgOut <- puzzleConfigurator PuzzleConfiguratorConfig
                    { _puzzleConfiguratorConfig_puzzle = puzzleData
                    , _puzzleConfiguratorConfig_knownTags = knownTags
                    , _puzzleConfiguratorConfig_huntMetas = knownMetas
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
            divClass "chat-sidebar" $ chatWidget "flex flex-col flex-grow p-4 overflow-y-scroll"
            elClass "pre" "commandOutput" $ dynText lastError
        , _framed_layout = \ (MenuSettings layout) tab -> (\t l -> if t == PuzzlePageTab_Chat then MutedChat else l) <$> tab <*> layout
        }

activeSolverList solvers =
          elClass "div" "ui simple dropdown item menuShrink" $ do
            let solverNames = Map.elems <$> solvers -- fmap (\(a :*: _) -> _user_name a) . Map.elems <$> solvers
            elClass "div" "ellipsisShrink" $ dynText $ "Active Users: " <> fmap (T.intercalate ",") solverNames
            -- elClass "i" "dropdown icon" blank
            elClass "div" "menu" $ do
              simpleList solverNames $ elClass "div" "item" . dynText


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
  return $ (<> statusTags) . fromMaybe mempty <$> tags
  

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
  puzzle <- traceShow "Entered puzzleBuilder" $ fmap (fmap (fmap getFirst)) $ watch $ (\puz -> key V_Puzzle ~> key puz) <$> puzIdD
  metas <- watch $ (\puzId -> key V_Metas ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD

  metas_puzzles <- fmap (fmap (fmap (fmap getFirst))) $ watch $ (\puz -> key V_Puzzle ~> keys (Map.keysSet puz)) . Map.mapKeys _meta_Metapuzzle . fromMaybe mempty <$> metas
  let metaIdTitle = fmap (fmap _puzzle_Title) $ fromMaybe mempty <$> metas_puzzles
  tags <- watch $ (\puzId -> key V_Tags ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  solutions <- watch $ (\puzId -> key V_Solutions ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  notes <- watch $ (\puzId -> key V_Notes ~> key puzId ~> postMap (traverse (fmap getMonoidalMap . getComplete))) <$> puzIdD
  let puzChan = (>>= fmap (ChatroomId @Identity) . unChatroomId . _puzzle_Channel) <$> puzzle
  dynCurrentDyn <- maybeDyn puzChan
  let currentSolverQuery = fmap (fmap ((\chanId -> key V_ActiveUsers ~> key chanId ~> postMap (traverse (fmap getMonoidalMap . getComplete))))) <$> dynCurrentDyn
  let watcher = fmap watch <$> currentSolverQuery
  dynRes <- dyn $ fromMaybe (return $ constDyn mempty) <$> watcher
  currentSolvers <- join <$> holdDyn (constDyn mempty) dynRes
  -- currentSolvers <- fromMaybe (constDyn mempty) . watch . 
  -- display puzzles
  return $ ffor puzzle $ \foundPuzD ->
    ffor foundPuzD $ \puz -> traceShow "PuzzleData built" $ PuzzleData
    { _puzzleData_puzzle = constDyn puz
    , _puzzleData_metas = metaIdTitle
    , _puzzleData_tags = (() <$) . Map.mapKeys _tagId_Tag <$> fromMaybe mempty <$> tags
    , _puzzleData_solutions = fromMaybe mempty <$> solutions
    , _puzzleData_notes = fromMaybe mempty <$> notes
    , _puzzleData_status = constDyn [""]
    , _puzzleData_currentSolvers = fromMaybe mempty <$> currentSolvers
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
  => Id Hunt -> m (Dynamic t (Map (Id Puzzle) (PuzzleDataT Identity)))
puzzleListBuilder huntIdD = do
  huntpuzzles <- traceShow "Entered puzzleListBuilder" $ fmap (fmap (fmap id)) $ watch $ (\hunt -> key V_HuntPuzzles ~> key hunt) <$> constDyn huntIdD
  puzzles_initial <- fmap (fromMaybe (SemiMap_Complete mempty)) $ sample $ current huntpuzzles
  huntpuzzlesDyn <- fmap (fmap knownKeysSet) $ foldDyn (<>) puzzles_initial $ fromMaybe mempty <$> updated huntpuzzles

  -- puzzle <- traceShow "Entered puzzleListBuilder" $ fmap (fmap (fmap getFirst)) $ 
  puzzlesD <- fmap (fmap (fmap getFirst . fromMaybe mempty)) $ watch $ (\puz -> key V_Puzzle ~> keys puz) <$> huntpuzzlesDyn
  -- let channelsD = ffor puzzlesD $ \puzMap ->
  --       Set.fromList $ Map.elems $ fmap ((ChatroomId . unChatroomId) . _puzzle_Channel) puzMap
  let channelsD :: Dynamic t (Set (Id Chatroom)) = ffor puzzlesD $ \puzMap -> Set.fromList $ Map.elems $ fmapMaybe id $ fmap (fmap ChatroomId . unChatroomId . _puzzle_Channel) puzMap
  metasD <- fmap (fmap (fromMaybe mempty)) $ watch $ (( (\puz -> key V_Metas ~> keys puz) <$> huntpuzzlesDyn ))

  tagsD <- fmap (fmap (fromMaybe mempty)) $ watch $ (\puz -> key V_Tags ~> keys puz) <$> huntpuzzlesDyn
  solutionsD <- fmap (fmap (fromMaybe mempty)) $ watch $ (\puz -> key V_Solutions ~> keys puz)  <$> huntpuzzlesDyn
  notesD <- fmap (fmap (fromMaybe mempty)) $ watch $ (\puz -> key V_Notes ~> keys puz)  <$> huntpuzzlesDyn
  currentSolversDF :: Dynamic t (Maybe (Map (Id Chatroom) (SemiMap (Id Account) Text))) <- watch $ (\chan -> key V_ActiveUsers ~> keys chan) <$> channelsD
  let currentSolversD = fmap (MMap.getMonoidalMap . fromMaybe mempty . getComplete) . fromMaybe mempty <$> currentSolversDF
  -- let curSD :: _ = fmap (fromMaybe mempty . fmap (fromMaybe mempty . getComplete)) <$> currentSolversDF
  -- let currentSolversD :: Dynamic t (Map (Id Chatroom) (Map (Id Account) Text)) = fmap (fmap getComplete) <$> currentSolversDF

  let combine puzzles metas tags solutions notes currentSolvers = ffor puzzles $ \puz -> -- id @(PuzzleDataT Identity) $
        let 
         puz_id = primaryKey puz
         puz_metas = Map.fromList $ fmap (\(mp, v) -> (_meta_Metapuzzle mp, fromMaybe "" $ fmap _puzzle_Title $ Map.lookup (_meta_Metapuzzle mp) puzzles)) $ MMap.toList $ fromMaybe mempty $ (>>= getComplete) $ Map.lookup puz_id metas
         puz_tags = Map.fromList $ fmap (\(k, _) -> (_tagId_Tag k, ())) $ MMap.toList $ fromMaybe MMap.empty $ (>>= getComplete) $ Map.lookup puz_id tags
         puz_sols = MMap.getMonoidalMap $ fromMaybe MMap.empty $ (>>= getComplete) $ Map.lookup puz_id solutions
         puz_notes = MMap.getMonoidalMap $ fromMaybe MMap.empty $ (>>= getComplete) $ Map.lookup puz_id notes
         puz_solvers = fromMaybe Map.empty $ (fmap ChatroomId $ unChatroomId $ _puzzle_Channel puz) >>= flip Map.lookup currentSolvers
         puz_status = [k | k <- Set.toList statusTags, Just () == Map.lookup k puz_tags ]
        in PuzzleData
         { _puzzleData_puzzle = puz
         , _puzzleData_metas = puz_metas
         , _puzzleData_tags = puz_tags
         , _puzzleData_solutions = puz_sols
         , _puzzleData_notes = puz_notes
         , _puzzleData_status = puz_status
         , _puzzleData_currentSolvers = puz_solvers
         }
      
    {- PuzzleData { _puzzleData_puzzle = puz
    , _puzzleData_metas = metaIdTitle
    , _puzzleData_tags = (() <$) . Map.mapKeys _tagId_Tag <$> fromMaybe mempty <$> tags
    , _puzzleData_solutions = fromMaybe mempty <$> solutions
    , _puzzleData_notes = fromMaybe mempty <$> notes
    , _puzzleData_status = constDyn [""]
    , _puzzleData_currentSolvers = fromMaybe mempty <$> currentSolvers
    } -}

  pure $ combine <$> puzzlesD <*> metasD <*> tagsD <*> solutionsD <*> notesD <*> currentSolversD

{-
puzzleListBuilderOld
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
  => Id Hunt -> m (Dynamic t (Map (Id Puzzle) (Dynamic t (PuzzleDataT Identity))))
puzzleListBuilderOld hunt = do
  puzzleIds <- watch $ pure $ key V_HuntPuzzles ~> key hunt ~> postMap (traverse (fmap getMonoidalMap . getComplete))
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
      , _puzzleData_status = constDyn [""]
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
-}
