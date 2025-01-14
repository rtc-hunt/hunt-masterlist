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

import Data.Patch.MapWithMove

import Common.Schema
import Frontend.Types
import Frontend.Utils
import Templates.Types
import Frontend.SortSelect
import Debug.Trace

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
tableDynAttrWithSearch :: forall t m r k v q. (Ord k, Adjustable t m, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Monoid q)
  => Text                                   -- ^ Class applied to <table> element
  -> [(Text, k -> r -> Event t r -> m v, m (Dynamic t q))]      -- ^ Columns of (header, row key -> row value -> child widget)
  -> Incremental t (PatchMapWithMove k r)                      -- ^ Map from row key to row value
  -> (k -> m (Dynamic t (Map Text Text))) -- ^ Function to compute <tr> element attributes from row key
  -> m (Dynamic t q, Dynamic t (Map k (Element EventResult (DomBuilderSpace m) t, [v])))        -- ^ Map from row key to (El, list of widget return values)
tableDynAttrWithSearch klass cols iRows rowAttrs = elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $
    elAttr "table" (Map.singleton "class" klass) $ do
      queryEls <- el "thead" $ do
        el "tr" $ mapM_ (\(h, _, _) -> el "th" $ text h) cols
        el "tr" $ mapM (\(_, _, qm) -> el "th" $ qm) cols
      initRows <- sample $ currentIncremental iRows
      --let qq :: _
      --     qq = fmap (fmap Nothing) $ Map.difference <$> current dRows <@> updated dRows
      -- let updateddRows = (fmap (fmap $ const Nothing) $ Map.difference <$> current dRows <@> updated dRows) <> (fmap Just <$> updatedIncremental dRows)
      bodyRes <- el "tbody" $ do
        let lster ir ur fun = mapMapWithAdjustWithMove fun ir ur
        -- listWithKeyShallowDiff initRows updateddRows $ (\k r er -> do
        lster initRows (updatedIncremental iRows) $ (\k r -> do
        -- listWithKey dRows (\k r -> do
           dAttrs <- rowAttrs k
           -- rowData <- holdDyn r er
           elDynAttr' "tr" dAttrs $ mapM (\(_, x, _) -> el "td" $ x k r undefined) cols)
      return (mconcat queryEls, undefined) -- bodyRes) 

backsolve1 :: DomBuilder t m => m ()
backsolve1 = elAttr "span" ("class" =: "tooltip" <> "style" =: "font-family: 'SymbolaRegular'" <> "data-tooltip" =: "This solution was backsolved.") $ do
  text " 🝡⃪"

headerDropdownSettings :: Reflex t => DropdownConfig t a
headerDropdownSettings = def & dropdownConfig_attributes .~ (constDyn $ "class" =: "grow shrink w-4/5 max-w-xs min-w-4")

puzzlesTable :: forall t m. (Template t m, MonadHold t m, MonadFix m) => 
  PuzzleTableConfig t m -> m ()
--  Dynamic t (Map (PrimaryKey Puzzle Identity) (Puzzle Identity)) -> m ()
puzzlesTable PuzzleTableConfig { _puzzleTableConfig_results = puzzles, _puzzleTableConfig_puzzleLink = puzzleLink, _puzzleTableConfig_metas = knownMetas, _puzzleTableConfig_tags = knownTags } = divClass "top-scrollable" $ mdo
          traceM "Starting puzzle table"
          {-queryEl <- inputElement $ def -- & inputElementConfig_initialValue .~ (T.pack $ show (mempty :: PuzzleQuery))
          el "br" blank
          let queries = fmap fst . (reads :: String -> [(PuzzleQuery, String)]) . T.unpack <$> _inputElement_value queryEl
          el "br" blank
          let query = Prelude.head <$> (queries <> ((:[]) <$> tblquery))
          display query
          el "br" blank -}
          --(tableQueryD :: Dynamic t ([Int]), tableResD) <- 
          -- jlet query = constDyn mempty
          uniqQuery <- holdDyn mempty $ updated query
          let puzzleDataDynamic = traceDynWith (\t -> show ("puzzleDataDynamic updated", Map.keys t)) $ puzzles -- (toSortKeys (_puzzleQuery_ordering <$> uniqQuery) $ prunePuzzles (_puzzleQuery_select <$> uniqQuery) $ puzzles)

          initialPuzzles <- sample $ current puzzleDataDynamic
--          let puzzleIdsD = imap (\i _ -> i) <$> puzzleDataDynamic
          puzzleIncremental <- holdIncremental initialPuzzles $ patchThatChangesMap <$> currentIncremental puzzleIncremental <@> updated puzzleDataDynamic
          
          {-display $ Map.keys <$> puzzleDataDynamic
          display $ join $ (sequenceA . fmap _puzzleData_puzzle) <$> puzzleDataDynamic
          display $ join $ (sequenceA . fmap _puzzleData_metas) <$> puzzleDataDynamic
          -- display $ join $ (sequenceA . fmap _puzzleData_solutions) <$> puzzleDataDynamic
          display $ join $ (sequenceA . fmap _puzzleData_tags) <$> puzzleDataDynamic
          -}
          -- display $ join $ (sequenceA . fmap _puzzleData_notes) <$> puzzleDataDynamic
          -- Reflex.Dom.Core.traceEvent $ "puzzleDataDynamic updated" <$ updated puzzleDataDynamic
          (query :: Dynamic t PuzzleQuery, _) <- traceShow "Puzzle list started" $ tableDynAttrWithSearch "puzzletable ui celled table"
            [ ("Title", \puzKey puzDat _ -> puzzleLink (primaryKey <$> (_puzzleData_puzzle puzDat)) $ elAttr "div" ("class" =: "" <> "data-tooltip" =: "Open Puzzle") $ dynText $ _puzzle_Title <$> (_puzzleData_puzzle puzDat), return $ (constDyn mempty))
            , ("Is meta?", \_ puzDat _ -> blank -- dynText $ (\p -> if p then "META" else "") . _puzzle_IsMeta <$> (puzDat >>= _puzzleData_puzzle)
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_IsMeta =: "Is Meta" <> (PuzzleSelect_Not PuzzleSelect_IsMeta) =: "Not Meta")) headerDropdownSettings
              )
            , ("Meta", \_ puzDat _ -> void $
                listWithKey (_puzzleData_metas puzDat) $ \k dV -> puzzleLink (constDyn k) $ dynText dV
                , elClass "span" "flex flex-row" $ do
                    queryByMeta <- fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_HasMeta <$> knownMetas) headerDropdownSettings
                    sortByMeta <- elClass "span" "flex-initial max-w-min" $ fmap (PuzzleQuery PuzzleSelect_All . (\a -> if a then PuzzleOrdering_ByMeta else PuzzleOrdering_Any)) <$> semToggle "" True
                    pure $ sortByMeta <> queryByMeta
                )
            , ("Solution(s)", \_ puzDat _ -> do
                dyn_ $ ffor (_puzzleData_solutions puzDat) $ \solMap -> forM_ (Map.toList solMap) $ \(solId, sol) -> do
                  el "pre" $ do 
                  text $ _solution_Solution sol
                  if _solution_IsBacksolve sol then backsolve1 else blank
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_HasSolution =: "Has Solution" <> (PuzzleSelect_Not PuzzleSelect_HasSolution) =: "No Solution")) headerDropdownSettings
              )
            , ("Status", \_ puzDat _ -> 
                void $ listWithKey (Map.filterWithKey (\k _ -> k `elem` statusTags) <$> (_puzzleData_tags puzDat)) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
               
            , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_WithTag . Map.fromSet (id) <$> constDyn statusTags) headerDropdownSettings
              )
            , ("Current Solvers", \_ puzDat _ -> 
                void $ listWithKey (_puzzleData_currentSolvers puzDat) $ \k u -> el "span" $ dynText u
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_HasSolvers =: "Has Current Solvers" <> (PuzzleSelect_Not PuzzleSelect_HasSolvers) =: "No Current Solvers")) headerDropdownSettings
              )
            , ("Voice Chat", \_ puzDat _ -> 
                let lnkD = _puzzle_voicelink <$> (_puzzleData_puzzle puzDat)
                in elDynAttr "a" (fromMaybe mempty . fmap ((<> ("class" =: "text-xs voicelink" <> "target" =: "_blank")) . ("href" =:)) <$> lnkD) $ dynText $ fromMaybe "" . ("Voice Chat" <$) <$> lnkD
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_HasVoice =: "Has Voice Chat" <> (PuzzleSelect_Not PuzzleSelect_HasVoice) =: "No Voice Chat")) headerDropdownSettings
              )
            , ("Tags", \_ puzDat _ ->
              void $ listWithKey (Map.filterWithKey (\k _ -> not $ k `elem` statusTags) <$> (_puzzleData_tags puzDat)) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_WithTag . Map.fromSet (id) <$> knownTags) headerDropdownSettings
              )
            , ("Notes", \_ puzDat _ ->
              void $ listWithKey (_puzzleData_notes puzDat) $ \k dV -> elClass "div" "" $ dynText $ _note_Note <$> dV
              , return $ constDyn mempty)
            ]
            puzzleIncremental
            (\k -> pure $ constDyn mempty)
          blank
