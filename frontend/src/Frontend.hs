{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Obelisk.Frontend
import Obelisk.Route
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
      --text "Welcome to Obelisk!"
      --el "p" $ text $ T.pack commonStuff
      --elAttr "img" ("src" =: static @"obelisk.jpg") blank
      _ <- runRhyoliteWidget functorToWire (httpToWs route) $ 
        puzzleMasterList
      return ()
  }

  where
    httpToWs url = fromMaybe "" $ (<> (renderBackendRoute checkedRouteEncoder $ BackendRoute_Listen :/ ())) . ("ws"<>) <$> T.stripPrefix "http" url

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

puzzleMasterList :: MonadRhyoliteWidget (HMLViewSelector SelectedCount) HMLRequest t m => m ()
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
      (elem) <- el' "td" $ elAttr "div" ("class" =: "tooltip pointer" <> "data-tooltip" =: "Open Puzzle") $ do
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


cssText :: T.Text
cssText = LT.toStrict $ Clay.render $ do
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
