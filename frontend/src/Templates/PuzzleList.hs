
module Templates.PuzzleList where

import Data.Default
import Data.Text
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Reflex.Dom.Core
import Database.Beam
import Control.Monad.Identity
import Obelisk.Route.Frontend

import Common.Schema
import Frontend.Types
import Templates.Types
import Frontend.SortSelect

data PuzzleTableConfig t m = PuzzleTableConfig
  { _puzzleTableConfig_results :: Dynamic t (Map (PrimaryKey Puzzle Identity) (PuzzleData t))
  , _puzzleTableConfig_puzzleLink :: Dynamic t (PrimaryKey Puzzle Identity) -> m () -> m ()
  }

data PuzzleTableOut t m = PuzzleTableOut
--  { _puzzleTableOut_
--  }

-- | A widget to display a table with static columns and dynamic rows.
tableDynAttrWithSearch :: forall t m r k v q. (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Monoid q)
  => Text                                   -- ^ Class applied to <table> element
  -> [(Text, k -> Dynamic t r -> m v, m (Dynamic t q))]      -- ^ Columns of (header, row key -> row value -> child widget)
  -> Dynamic t (Map k r)                      -- ^ Map from row key to row value
  -> (k -> m (Dynamic t (Map Text Text))) -- ^ Function to compute <tr> element attributes from row key
  -> m (Dynamic t q, Dynamic t (Map k (Element EventResult (DomBuilderSpace m) t, [v])))        -- ^ Map from row key to (El, list of widget return values)
tableDynAttrWithSearch klass cols dRows rowAttrs = elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $
    elAttr "table" (Map.singleton "class" klass) $ do
      queryEls <- el "thead" $ do
        el "tr" $ mapM_ (\(h, _, _) -> el "th" $ text h) cols
        el "tr" $ mapM (\(_, _, qm) -> el "th" $ qm) cols
      bodyRes <- el "tbody" $
        listWithKey dRows (\k r -> do
          dAttrs <- rowAttrs k
          elDynAttr' "tr" dAttrs $ mapM (\(_, x, _) -> el "td" $ x k r) cols)
      return (mconcat queryEls, bodyRes) 

backsolve1 :: DomBuilder t m => m ()
backsolve1 = elAttr "span" ("class" =: "tooltip" <> "style" =: "font-family: 'SymbolaRegular'" <> "data-tooltip" =: "This solution was backsolved.") $ do
  text " 🝡⃪"

puzzlesTable :: forall t m. (Template t m, MonadHold t m, MonadFix m) => 
  PuzzleTableConfig t m -> m ()
--  Dynamic t (Map (PrimaryKey Puzzle Identity) (Puzzle Identity)) -> m ()
puzzlesTable PuzzleTableConfig { _puzzleTableConfig_results = puzzles, _puzzleTableConfig_puzzleLink = puzzleLink } = divClass "top-scrollable" $ mdo
          --(tableQueryD :: Dynamic t ([Int]), tableResD) <- 
          (query :: Dynamic t PuzzleQuery, _) <- tableDynAttrWithSearch "puzzletable ui celled table"
            [ ("Title", \puzKey puzDat -> puzzleLink (primaryKey <$> (puzDat >>= _puzzleData_puzzle)) $ elAttr "div" ("class" =: "" <> "data-tooltip" =: "Open Puzzle") $ dynText $ _puzzle_Title <$> (puzDat >>= _puzzleData_puzzle), return $ (constDyn mempty))
            , ("Is meta?", \_ puzDat -> dynText $ (\p -> if p then "META" else "") . _puzzle_IsMeta <$> (puzDat >>= _puzzleData_puzzle), return $ constDyn mempty)
            , ("Meta", \_ puzDat -> void $
                listWithKey (puzDat >>= _puzzleData_metas) $ \k dV -> puzzleLink (constDyn k) $ dynText dV
                , return $ constDyn mempty)
            , ("Solution(s)", \_ puzDat -> do
                dyn_ $ ffor (puzDat >>= _puzzleData_solutions) $ \solMap -> forM_ (Map.toList solMap) $ \(solId, sol) -> do
                  el "pre" $ do 
                  text $ _solution_Solution sol
                  if _solution_IsBacksolve sol then backsolve1 else blank
              , return $ constDyn mempty
              )
            , ("Status", \_ puzData -> dynText $ (puzData >>= _puzzleData_status), return $ constDyn mempty)
            , ("Current Solvers", \_ puzDat -> 
                void $ listWithKey (puzDat >>= _puzzleData_currentSolvers) $ \k u -> el "span" $ dynText u
              , return $ constDyn mempty)
            , ("Voice Chat", \_ puzDat -> 
                let lnkD = _puzzle_voicelink <$> (puzDat >>= _puzzleData_puzzle)
                in elDynAttr "a" (fromMaybe mempty . fmap ((<> ("class" =: "text-xs voicelink" <> "target" =: "_blank")) . ("href" =:)) <$> lnkD) $ dynText $ fromMaybe "" . ("Voice Chat" <$) <$> lnkD
              , return $ constDyn mempty)
            , ("Tags", \_ puzDat ->
                void $ listWithKey (puzDat >>= _puzzleData_tags) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
               , return $ constDyn mempty)
            , ("Notes", \_ puzDat ->
                void $ listWithKey (puzDat >>= _puzzleData_notes) $ \k dV -> elClass "div" "" $ dynText $ _note_Note <$> dV
              , return $ constDyn mempty)
            ]
            (toSortKeys (_puzzleQuery_ordering <$> query) $ prunePuzzles (_puzzleQuery_select <$> query) $ puzzles)
            (\k -> pure $ constDyn mempty)
          blank
