{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Dependent.Sum
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Control.Monad
import Data.Map.Monoidal
import Data.Maybe
import qualified Data.Map.Monoidal as MMap
import Database.Id.Class
import Rhyolite.Frontend.App
import Rhyolite.SemiMap

import Common.Schema

import Common.Api
import Common.App
import Common.Route
import Obelisk.Generated.Static
import Obelisk.Configs
import Clay
import Debug.Trace


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
     el "title" $ text "Obelisk Minimal Example"
     elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://fontlibrary.org/face/symbola" <> "type" =: "text/css") $ blank
     -- elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css" <> "type" =: "text/css") $ blank
     elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/components/table.min.css" <> "type" =: "text/css") $ blank
     elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/components/popup.min.css" <> "type" =: "text/css") $ blank
     elAttr "link" ( "rel" =: "stylesheet" <> "media" =: "screen" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/components/icon.min.css" <> "type" =: "text/css") $ blank
  , _frontend_body = do
      el "style" $ text cssText
      route <- fromMaybe "" <$> getTextConfig "common/route"
      trace (show route) $ return ()
      --text "Welcome to Obelisk!"
      --el "p" $ text $ T.pack commonStuff
      --elAttr "img" ("src" =: static @"obelisk.jpg") blank
      runApp $ 
        subRoute_ $ \case
          FrontendRoute_Main -> puzzleMasterList
          FrontendRoute_Puzzle -> puzzlePage
      return ()
  }

  where
    -- runApp :: (MonadWidget t m) => T.Text -> RhyoliteWidget (HMLViewSelector SelectedCount) HMLRequest t m a -> m a
    runApp
      :: (ObeliskWidget js t route m)
      -- -> RhyoliteWidget (HMLViewSelector SelectedCount) HMLRequest t m a
      -- -> Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
      -- -> R BackendRoute
      => RoutedT t (R FrontendRoute) (RhyoliteWidget (HMLViewSelector SelectedCount) HMLRequest t m) a
      -> RoutedT t (R FrontendRoute) m a
    runApp = runObeliskRhyoliteWidget functorToWire "common/route" checkedRouteEncoder (BackendRoute_Listen :=> Identity ())

    httpToWs url = fromMaybe "" $ (<> (renderBackendRoute checkedRouteEncoder $ BackendRoute_Listen :/ ())) . ("ws"<>) <$> T.stripPrefix "http" url

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

puzzleMasterList :: (RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, MonadRhyoliteWidget (HMLViewSelector SelectedCount) HMLRequest t m) => m ()
puzzleMasterList = do
  puzzles <- watchPuzzles
  elClass "table" "ui celled table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ text "Title"
      --el "th" $ text "Puzzle Page"
      --el "th" $ text "Worksheet"
      el "th" $ text "Is Meta?"
      el "th" $ text "Meta"
      el "th" $ text "Solution(s)"
      el "th" $ text "Status"
      el "th" $ text "Current solvers"
      el "th" $ text "Tags"
      elAttr "th" ("style" =: "width: 40%") $ text "Notes"


    dyn_ $ ffor puzzles $ \pzlMap -> forM_ (MMap.toList pzlMap) $ \(pzlId, pzl) -> el "tr" $ do
      el "td" $ routeLink (FrontendRoute_Puzzle ==> pzlId) $ elAttr "div" ("class" =: "tooltip pointer" <> "data-tooltip" =: "Open Puzzle") $ do
        text $ _puzzle_Title pzl
        -- elClass "span" "tooltiptext" $ text "Open Puzzle"
      --el "td" $ elAttr "a" ("href" =: _puzzle_URI pzl) $ text "Puzzle Page"
      --el "td" $ elAttr "a" ("href" =: "#puzzle" <> "style" =: "font-family 'SymbolaRegular'") $ text "Sheet"
      el "td" $ if _puzzle_IsMeta pzl then text "META" else text ""
      el "td" $ text "Unknown" -- if _puzzle_IsMeta pzl then text "META" else text ""
      el "td" $ do
        solutions <- watchSolves $ constDyn $ SolveQuery_byPuzzle $ pzlId
        dyn_ $ ffor solutions $ \solMap -> forM_ (MMap.toList solMap) $ \(solId, sol) -> do
          el "pre" $ do 
            text $ _solve_Solution sol
            if _solve_IsBacksolve sol then backsolve1 else blank
      el "td" $ text $ maybe "Not Started" tshow $ _puzzle_StartedAt pzl
      el "td" $ do
        text "Nobody"
      el "td" $ do
        _ <- elClass' "i" "plus icon" $ blank
        blank
      el "td" $ do 
        el "div" $ text "a normal note"
        elClass "div" "help-wanted" $ text "cryptics"
  return ()


puzzlePage :: (RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Routed t (Id Puzzle) m, MonadRhyoliteWidget (HMLViewSelector SelectedCount) HMLRequest t m) => m ()
puzzlePage = divClass "pzl-container" $ do
  puz <- askRoute
  puzzleData <- watchPuzzle puz
  dyn_ $ ffor puzzleData $ \case
    Just pzl -> do
      elClass "span" "header" $ do
        routeLink (FrontendRoute_Main ==> ()) $ text "Back"
        el "h3" $ text $ _puzzle_Title $ pzl
      
      divClass "chatDropdown" $ do
        text "This is some kind of chat dropdown thingie, that will shift itself up to the top of teh screen."

      tabDisplay "tabbar" "activeTab" $
        "puzzle" =: ("puzzle", do
          divClass "framed" $ elAttr "iframe" ("src" =: _puzzle_URI pzl <> "width" =: "100%" <> "height" =: "100vw") $ blank
          )
        <> "sheet" =: ("sheet", do
          divClass "framed" $ elAttr "iframe" ("src" =: fromMaybe "" (_puzzle_SheetURI pzl) <> "width" =: "100%" <> "height" =: "100vw") $ blank
          )
    Nothing -> blank


cssText :: T.Text
cssText = LT.toStrict $ Clay.render $ do
  ".chatDropdown" ? do
    Clay.display Clay.block
    Clay.position Clay.absolute
    Clay.width (Clay.vw 40)
    Clay.right (Clay.em 0)
    Clay.height (Clay.vh 5)
    Clay.transitionProperty "height"
    Clay.transitionDuration $ sec 1
    Clay.padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    Clay.borderStyle Clay.solid
    Clay.borderWidth (px 1)
    Clay.borderRadius (em 1) (em 1) (em 1) (em 1)
    Clay.borderColor Clay.black
  ".chatDropdown" # ":hover" ? do
    Clay.height (Clay.vh 90)
  ".header" ? do
    Clay.display flex
    Clay.flexDirection Clay.row
  ".header" |> Clay.h3 ? do
    Clay.margin (px 0) (px 0) (px 0) (px 0)
  ".pzl-container" ? do
    Clay.display Clay.flex
    Clay.flexDirection Clay.column
    Clay.height (Clay.vh 96)
  ".pzl-container" |> Clay.div ? do
    Clay.flexGrow 1
    -- Clay.backgroundColor Clay.orange
    Clay.backgroundColor Clay.white
    Clay.div ?
      Clay.height (Clay.pct 100)
  ".framed" ? do
    Clay.backgroundColor Clay.lightgray
  Clay.iframe ? do
    Clay.width (Clay.pct 100)
    Clay.height (Clay.pct 100)
  ".tabbar" ? do
    Clay.listStyleType Clay.none
  ".tabbar" |> Clay.li ? do
    Clay.display Clay.inlineBlock
    Clay.padding (em 0) (em 1) (em 0) (em 1)
    Clay.borderRadius (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    Clay.backgroundColor Clay.lightgray
    cursor pointer
  ".tabbar" |> Clay.li # ".activeTab" ? do
    Clay.backgroundColor Clay.gray
  ".pointer" ? cursor pointer
  ".tooltip" ? do
    Clay.position relative
  ".tooltip" # ":hover" |> ".tooltiptext" ? do
    Clay.display block
    Clay.position absolute
    Clay.background black
    Clay.fontColor white
    Clay.fontSize (pt 7)
    Clay.padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    Clay.borderRadius (em 0.5) (em 0.5) (em 0.5) (em 0.5)
  ".tooltip" |> ".tooltiptext" ? do
    Clay.display none
    Clay.position absolute
  pre ? do
    Clay.padding (em 0) (em 0) (em 0) (em 0)
    Clay.margin (em 0) (em 0) (em 0) (em 0)
  Clay.div # ".help-wanted" ? do
    Clay.borderRadius (em 0.5) (em 0.5) (em 0.5) (em 0.5)
    Clay.background (rgb 255 220 220)
    Clay.padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
  Clay.div # ".help-wanted" # "::before" ? Clay.content (stringContent "HELP: ")
  Clay.i ? do
    Clay.fontColor lightgray


backsolve1 :: DomBuilder t m => m ()
backsolve1 = elAttr "span" ("class" =: "tooltip" <> "style" =: "font-family: 'SymbolaRegular'" <> "data-tooltip" =: "This solution was backsolved.") $ do
  text " 🝡⃪"
--  elClass "span" "tooltiptext" $ text "This solution was backsolved."

backsolve2 :: DomBuilder t m => m ()
backsolve2 = elAttr "span" ("style" =: "font-family: 'SymbolaRegular'") $ do
   text " 🝢⃪"


watchPuzzles :: (Reflex t, MonadQuery t (HMLViewSelector SelectedCount) m, MonadHold t m, MonadFix m)
  => m (Dynamic t (MonoidalMap (Id Puzzle) Puzzle))
watchPuzzles = do
  let q = constDyn PuzzleQuery_AllPuzzles
  dynV <- watchViewSelector $ ffor q $ \q' -> mempty
    { _hmlViewSelector_puzzle = MMap.singleton q' (1 :: SelectedCount) }
  let queryRes = (MMap.lookup PuzzleQuery_AllPuzzles . _hmlView_puzzle) <$> dynV
      mQueryResMap = ffor queryRes $ \case
        Just (viewVal :: (SelectedCount, SemiMap (Id Puzzle) Puzzle)) -> getComplete $ snd viewVal
        Nothing -> Nothing
  return $ fmap (fromMaybe MMap.empty) mQueryResMap

watchPuzzle :: (Reflex t, MonadQuery t (HMLViewSelector SelectedCount) m, MonadHold t m, MonadFix m)
  => Dynamic t (Id Puzzle) -> m (Dynamic t (Maybe Puzzle))
watchPuzzle pzl = do
  let q = PuzzleQuery_byId <$> pzl
  dynV <- watchViewSelector $ ffor q $ \q' -> mempty
    { _hmlViewSelector_puzzle = MMap.singleton q' (1 :: SelectedCount) }
  let queryRes = MMap.lookup <$> q <*> (_hmlView_puzzle <$> dynV)
      mQueryResMap = ffor queryRes $ \case
        Just (viewVal :: (SelectedCount, SemiMap (Id Puzzle) Puzzle)) -> getComplete $ snd viewVal
        Nothing -> Nothing
  return $ MMap.lookup <$> pzl <*> fmap (fromMaybe MMap.empty) mQueryResMap


watchSolves :: (Reflex t, MonadQuery t (HMLViewSelector SelectedCount) m, MonadHold t m, MonadFix m)
  => Dynamic t SolveQuery -> m (Dynamic t (MonoidalMap (Id Solve) Solve))
watchSolves q = do
  dynV <- watchViewSelector $ ffor q $ \q' -> mempty
    { _hmlViewSelector_solve = MMap.singleton q' (1 :: SelectedCount) }
  let queryRes = MMap.lookup <$> q <*> (_hmlView_solve <$> dynV)
      mQueryResMap = ffor queryRes $ \case
        Just (viewVal :: (SelectedCount, SemiMap (Id Solve) Solve)) -> getComplete $ snd viewVal
        Nothing -> Nothing
  return $ fmap (fromMaybe MMap.empty) mQueryResMap
