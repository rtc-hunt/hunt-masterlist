{-# Language OverloadedLists #-}
module Templates.PuzzleList where

import Data.Default
import Data.Text
import Data.Text as T
import Data.Map (Map)
import Data.Map as Map
import Data.Set
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
  , _puzzleTableConfig_metas :: Dynamic t (Map (Id Puzzle) Text)
  , _puzzleTableConfig_tags :: Dynamic t (Set Text)
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
puzzlesTable PuzzleTableConfig { _puzzleTableConfig_results = puzzles, _puzzleTableConfig_puzzleLink = puzzleLink, _puzzleTableConfig_metas = knownMetas, _puzzleTableConfig_tags = knownTags } = divClass "top-scrollable" $ mdo
          {-queryEl <- inputElement $ def -- & inputElementConfig_initialValue .~ (T.pack $ show (mempty :: PuzzleQuery))
          el "br" blank
          let queries = fmap fst . (reads :: String -> [(PuzzleQuery, String)]) . T.unpack <$> _inputElement_value queryEl
          el "br" blank
          let query = Prelude.head <$> (queries <> ((:[]) <$> tblquery))
          display query
          el "br" blank -}
          --(tableQueryD :: Dynamic t ([Int]), tableResD) <- 
          (query :: Dynamic t PuzzleQuery, _) <- tableDynAttrWithSearch "puzzletable ui celled table"
            [ ("Title", \puzKey puzDat -> puzzleLink (primaryKey <$> (puzDat >>= _puzzleData_puzzle)) $ elAttr "div" ("class" =: "" <> "data-tooltip" =: "Open Puzzle") $ dynText $ _puzzle_Title <$> (puzDat >>= _puzzleData_puzzle), return $ (constDyn mempty))
            , ("Is meta?", \_ puzDat -> dynText $ (\p -> if p then "META" else "") . _puzzle_IsMeta <$> (puzDat >>= _puzzleData_puzzle)
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_IsMeta =: "Is Meta" <> (PuzzleSelect_Not PuzzleSelect_IsMeta) =: "Not Meta")) def
              )
            , ("Meta", \_ puzDat -> void $
                listWithKey (puzDat >>= _puzzleData_metas) $ \k dV -> puzzleLink (constDyn k) $ dynText dV
                , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_HasMeta <$> knownMetas) def
                )
            , ("Solution(s)", \_ puzDat -> do
                dyn_ $ ffor (puzDat >>= _puzzleData_solutions) $ \solMap -> forM_ (Map.toList solMap) $ \(solId, sol) -> do
                  el "pre" $ do 
                  text $ _solution_Solution sol
                  if _solution_IsBacksolve sol then backsolve1 else blank
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_HasSolution =: "Has Solution" <> (PuzzleSelect_Not PuzzleSelect_HasSolution) =: "No Solution")) def
              )
            , ("Status", \_ puzData -> dynText $ (puzData >>= _puzzleData_status)
            , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_WithTag . Map.fromSet (id) <$> constDyn statusTags) def
              )
            , ("Current Solvers", \_ puzDat -> 
                void $ listWithKey (puzDat >>= _puzzleData_currentSolvers) $ \k u -> el "span" $ dynText u
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_HasSolvers =: "Has Current Solvers" <> (PuzzleSelect_Not PuzzleSelect_HasSolvers) =: "No Current Solvers")) def
              )
            , ("Voice Chat", \_ puzDat -> 
                let lnkD = _puzzle_voicelink <$> (puzDat >>= _puzzleData_puzzle)
                in elDynAttr "a" (fromMaybe mempty . fmap ((<> ("class" =: "text-xs voicelink" <> "target" =: "_blank")) . ("href" =:)) <$> lnkD) $ dynText $ fromMaybe "" . ("Voice Chat" <$) <$> lnkD
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_HasVoice =: "Has Voice Chat" <> (PuzzleSelect_Not PuzzleSelect_HasVoice) =: "No Voice Chat")) def
              )
            , ("Tags", \_ puzDat ->
                void $ listWithKey (puzDat >>= _puzzleData_tags) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_WithTag . Map.fromSet (id) <$> knownTags) def
              )
            , ("Notes", \_ puzDat ->
                void $ listWithKey (puzDat >>= _puzzleData_notes) $ \k dV -> elClass "div" "" $ dynText $ _note_Note <$> dV
              , return $ constDyn mempty)
            ]
            (toSortKeys (_puzzleQuery_ordering <$> query) $ prunePuzzles (_puzzleQuery_select <$> query) $ puzzles)
            (\k -> pure $ constDyn mempty)
          blank
