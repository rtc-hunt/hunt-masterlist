{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Frontend.Puzzle where

import qualified Data.Time.Clock

import Control.Monad
import Control.Monad.Fix
import Control.Lens.Indexed
import Data.Coerce
import Control.Applicative
import Control.Monad.IO.Class
import Control.Lens
import Data.Semigroup (Endo(..))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Either
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap
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
import Rhyolite.Vessel.Path as P
import Data.Map.Monoidal as MMap hiding (keys)
import Data.Functor.Misc (Const2(..))
import Rhyolite.SemiMap

import Data.Patch.MapWithMove

import Control.Monad.Zip

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
import Frontend.Patch

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
  ) => m (Event t ())
puzzles = do
  routeD <- askRoute
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
              in fmap (domEvent Click . fst) $ elDynAttr' "div" itemClass $ do
                       text $ masterlistPageToText tab
                       case tab of
                         MasterlistPage_HuntPage -> do
                            let lnk = _hunt_rootpage <$> hunt
                                lattr = ((\l -> ("target":: Text) =: "_blank" <> "href" =: l) <$> lnk)
                            elDynAttr "a" lattr $ elClass "i" "external alternate icon px-4" $ blank
                         _ -> blank
            activeTab :: Dynamic t MasterlistPage <- holdDyn MasterlistPage_List $ leftmost $ zipWith (<$) [MasterlistPage_List, MasterlistPage_HuntPage, MasterlistPage_Chat] evts
            -- activeSolverList puzId puzzlesData
            return $ activeTab
    , _framed_body = \(activeTab :: Dynamic t MasterlistPage) msgString cmdString _ -> do
        let frameURI uriD = divClass "framed" $ elDynAttr "iframe" (("src" =: ) <$> uriD) blank
        let mcid = (Just) <$> (_hunt_channel <$> hunt)
            chatWidget cls = do
                   channelView <- channelBuilder mcid
                   void $ do -- prerender blank $ do
                    rec (msgs, _) <- elAttr' "div" ("class" =: cls <> "style" =: "height: 100%; flex-direction: column-reverse; display: flex;") $ divClass "flex-grow flex flex-col" $ do
                          dyn_ $ ffor (_channelView_messages channelView) $ \case
                            Nothing -> text "No messages"
                            Just ms -> void $ listWithKey ms $ \_ -> Templates.message
                    blank
            mkMsgReq c m = case c of
                        Nothing -> Nothing
                        Just c' -> Just $ ApiRequest_Private () $ PrivateRequest_SendMessage c' m
        chatOverlay False mcid
        _ <- requestingIdentity $ attachWithMaybe (\c m -> mkMsgReq c m) (current mcid) msgString
        
        myTabDisplay "ui top attached tabular menu" "activeTab" activeTab $
          MasterlistPage_List =: ("List", do
            puzzleListI <- puzzleListBuilder huntId
            performEvent_ $ (liftIO (Data.Time.Clock.getCurrentTime >>= (print . ((,) "puzzleListD updated: ")))) <$ updatedIncremental puzzleListI
            puzzlesTable PuzzleTableConfig
              { _puzzleTableConfig_query = queryD
              , _puzzleTableConfig_modifyQuery = \e -> do
                   modifyRoute $ (\(Endo mod) -> \case
                      FrontendRoute_Puzzle :/ (hunt, currentQuery) -> FrontendRoute_Puzzle :/ (hunt, over _Left mod $ currentQuery)
                      a -> a) <$> e
              , _puzzleTableConfig_results = puzzleListI
              , _puzzleTableConfig_puzzleLink = \id -> reloadingRouteLink $ (\i -> FrontendRoute_Puzzle :/ (huntId, Right i)) <$> id
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
              elClass "form" "ui form" $ divClass "inline fields" $ {- prerender_ (return ()) $ -} mdo
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
      -- el "li" $ dynRouteLink ((\a -> FrontendRoute_Puzzle :/ (HuntId $ _hunt_id a, Left mempty)) <$> dHunt) $ elClass "div" "text-lg huntlist-button" $ dynText $ _hunt_title <$> dHunt
      el "li" $ reloadingRouteLink ((\a -> FrontendRoute_Puzzle :/ (HuntId $ _hunt_id a, Left mempty)) <$> dHunt) $ elClass "div" "text-lg huntlist-button" $ dynText $ _hunt_title <$> dHunt
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
            chatOverlay True $ fmap ChatroomId . unChatroomId . _puzzle_Channel <$> (puzzleData >>= _puzzleData_puzzle) -- divClass "chat-overlay scrollable" $ text "Chat Messagez"
            let mcid = (fmap ChatroomId . unChatroomId . _puzzle_Channel) <$> (puzzleData >>= _puzzleData_puzzle)
                chatWidget cls = do
                       channelView <- channelBuilder mcid
                       void $ {- prerender blank $ -} do
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
    -- { _puzzleData_id = primaryKey puz
    { _puzzleData_puzzle = constDyn puz
    , _puzzleData_metas = metaIdTitle
    , _puzzleData_tags = (() <$) . Map.mapKeys _tagId_Tag <$> fromMaybe mempty <$> tags
    , _puzzleData_solutions = fromMaybe mempty <$> solutions
    , _puzzleData_notes = fromMaybe mempty <$> notes
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
  -- => Id Hunt -> m (Incremental t (PatchMapWithPatchingMove (Id Puzzle) PuzzleDataPatch)) -- (Dynamic t (StrictMap.Map (Id Puzzle) (PuzzleDataT Identity)))
  => Id Hunt -> m (Incremental t (PatchMapWithMove (Id Puzzle) (PuzzleDataT Identity)))-- m (Dynamic t (StrictMap.Map (Id Puzzle) (PuzzleData t)))
puzzleListBuilder hunt = do
  puzzleIds <- watch $ pure $ key V_HuntPuzzles ~> key hunt ~> postMap (traverse (fmap getMonoidalMap . getComplete))

  puzzleIds2 <- fmap (fmap (Map.keysSet . fromMaybe mempty)) $ watch $ pure $ key V_HuntPuzzles ~> key hunt ~> postMap (traverse (fmap getMonoidalMap . getComplete))
  channels <- fmap (fmap $ fromMaybe mempty) $ watch $ ffor puzzleIds2 $ \puzs -> key V_Puzzle ~> keys puzs ~> postMap (Just . fmap (Set.fromList . fmapMaybe (\case { ChatroomId Nothing -> Nothing; ChatroomId (Just k) -> Just $ (ChatroomId k :: Id Chatroom) }) . Map.elems . fmap (_puzzle_Channel . getFirst))) -- (Map.keysSet (fromMaybe mempty puzs))

  let bzip :: (Coercible a a', Semigroup c) => Path a c c' d -> Path a' c c' d' -> Path a c c' (d, d')
      bzip (Path to from) (Path to' from') = Path (\x -> to x <> to' (coerce x)) (\c -> liftA2 (,) (from c) (from' c))
      infixr 9 `zz`
      zz :: (Coercible a a', Semigroup c) => Path a c c' (Identity d) -> Path a' c c' (Identity d') -> Path a c c' (Identity (d, d'))
      a `zz` b = bzip a b ~> postMap (Just . uncurry mzip)
      infixr 9 `zz2`
      zz2 :: (Coercible a a', Semigroup c, Ord k) => Path a c c' (Identity (Map k d)) -> Path a' c c' (Identity (Map k d')) -> Path a c c' (Identity (Map k (d, d')))
      a `zz2` b = bzip a b ~> postMap (Just . fmap (uncurry (Map.intersectionWith (,))) . uncurry mzip)
      -- allTheThingsQuery :: Dynamic t (Set (Id Puzzle)) -> Dynamic t (Set (Id Chatroom)) -> Path (Const SelectedCount _) _ _ _
      allTheThingsQuery puzs chans = 
       (
        ((key V_Puzzle ~> keys puzs)
        `zz2` (key V_Metas ~> keys puzs)
        `zz2` (key V_Tags ~> keys puzs)
        `zz2` (key V_Solutions ~> keys puzs)
        `zz2` (key V_Notes ~> keys puzs))
        `zz` ((key V_ActiveUsers ~> keys chans))
        ) ~> postMap (Just . fmap (\(puzs, users) ->
           ffor puzs $ \(puz, (metas, (tags, (sols, notes)))) ->
              let activeUsers = case _puzzle_Channel $ getFirst puz of
                    ChatroomId Nothing -> Nothing
                    ChatroomId (Just c) -> Map.lookup (ChatroomId c) users
              in (puz, Map.mapKeys (_meta_Metapuzzle) $ imap (\k _ -> fromMaybe "ERROR GETTING META TITLE" $ fmap (_puzzle_Title . getFirst . fst) $ Map.lookup (_meta_Metapuzzle k) puzs) $ getMonoidalMap $ fromMaybe mempty $ getComplete metas, tags, sols, notes, fromMaybe mempty $ activeUsers)
        ))
  let queryD = allTheThingsQuery <$> puzzleIds2 <*> channels
  puzDataD <- watch $ queryD

  let 
   puzzlesD = ffor puzDataD $ \puzzles -> (flip Map.mapWithKey) (fromMaybe mempty puzzles) $ \puzId (puz, metas, tags, solutions, notes, currentSolvers) ->
    PuzzleData
      -- { _puzzleData_id = puzId
      { _puzzleData_puzzle = getFirst puz
      , _puzzleData_metas = metas -- (constDyn $ getMonoidalMap $ fromMaybe mempty $ getComplete metas)
      , _puzzleData_tags = Map.mapKeys _tagId_Tag $ fmap (const ()) $ fromMaybe mempty $ fmap getMonoidalMap $ getComplete $ tags
      , _puzzleData_solutions = fromMaybe mempty $ fmap getMonoidalMap $ getComplete $ solutions
      , _puzzleData_notes = fromMaybe mempty $ fmap getMonoidalMap $ getComplete $ notes
      , _puzzleData_currentSolvers = fromMaybe mempty $ fmap getMonoidalMap $ getComplete $ currentSolvers
      }

  initialPuzzles <- sample $ current puzzlesD
  traceM "Building puzzle list"
  rec
    --puzzleIncremental <- holdIncremental initialPuzzles $ patchThatChangesMapWith PuzzleOrd diffPuzzleData <$> currentIncremental puzzleIncremental <@> updated puzzlesD
    puzzleIncremental <- holdIncremental initialPuzzles $ patchThatChangesMap <$> current puzzlesD <@> updated puzzlesD

  return puzzleIncremental 
  --return puzzlesD


