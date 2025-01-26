{-# Language OverloadedLists #-}
{-# Language TypeApplications #-}
module Templates.PuzzleList where

import Control.Concurrent
import Control.Concurrent.Thread.Delay as Concurrent
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
import Data.Time.Clock (UTCTime(..))
import Data.Time

import Data.Patch.MapWithPatchingMove

import Data.Int
import Data.Coerce
import Database.Beam.Backend.SQL.Types

import Data.Patch.MapWithMove

import Common.Schema
import Common.Route
import Data.Semigroup (Endo(..))
import Frontend.Types
import Frontend.Utils
import Templates.Types
import Frontend.SortSelect
import Debug.Trace hiding (traceEvent)

data PuzzleTableConfig t m = PuzzleTableConfig
  { _puzzleTableConfig_query :: Dynamic t PuzzleQuery
  , _puzzleTableConfig_modifyQuery :: Event t (Endo PuzzleQuery) -> m ()
  --, _puzzleTableConfig_results :: Incremental t (PatchMapWithPatchingMove (Id Puzzle) PuzzleDataPatch)-- Dynamic t (Map (PrimaryKey Puzzle Identity) (PuzzleDataT Identity))
  , _puzzleTableConfig_results :: Incremental t (PatchMapWithMove (PuzzleSortKey) (PuzzleDataT Identity))-- Dynamic t (Map (PrimaryKey Puzzle Identity) (PuzzleDataT Identity))
  , _puzzleTableConfig_puzzleLink :: Dynamic t (PrimaryKey Puzzle Identity) -> m () -> m ()
  , _puzzleTableConfig_metas :: Dynamic t (Map (Id Puzzle) Text)
  , _puzzleTableConfig_tags :: Dynamic t (Set Text)
  }

data PuzzleTableOut t m = PuzzleTableOut
--  { _puzzleTableOut_
--  }

-- | A widget to display a table with static columns and dynamic rows.
tableDynAttrWithSearch :: forall t m r k v q rp. (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Monoid q, Show k, MonadIO (Performable m), TriggerEvent t m, PerformEvent t m)
  => Text                                   -- ^ Class applied to <table> element
  -> [(Text, k -> Dynamic t r -> m v, m (Dynamic t q))]      -- ^ Columns of (header, row key -> row value -> child widget)
--  -> Incremental t (PatchMapWithPatchingMove k rp)                      -- ^ Map from row key to row value
  -> Incremental t (PatchMapWithMove k r)                      -- ^ Map from row key to row value
  -> (k -> r -> m (Dynamic t (Map Text Text))) -- ^ Function to compute <tr> element attributes from row key
  -> m (Dynamic t q, Dynamic t (Map k (Element EventResult (DomBuilderSpace m) t, [v])))        -- ^ Map from row key to (El, list of widget return values)
tableDynAttrWithSearch klass cols iRows rowAttrs = elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $
    elAttr "table" (Map.singleton "class" klass) $ do
      queryEls <- el "thead" $ do
        el "tr" $ mapM_ (\(h, _, _) -> el "th" $ text h) cols
        el "tr" $ mapM (\(_, _, qm) -> el "th" $ qm) cols
{- <<<<<<< HEAD
      bodyRes <- el "tbody" $ mdo
        let startEvt = (() <$ updated dRows) <> (() <$ updated (mconcat queryEls))
        let shouldStopD = (>) <$> count <*> (max 24 . Map.size <$> dRows)
        let stopEvt = () <$ (gate (current shouldStopD) ticks )
        tickerD :: Dynamic t (m (Event t TickInfo)) <- holdDyn (pure never) $ leftmost [ ((tickLossy 0.5 (UTCTime (toEnum 0) 0)) <$ startEvt), (pure never) <$ traceEvent "stopevent" stopEvt ]
        -- performEvent_ $ liftIO (putStrLn "Start") <$ startEvt
        -- performEvent_ $ liftIO (putStrLn "Stop") <$ stopEvt
        -- ticks <- tickLossyUpTo $ (\c -> (1, (UTCTime (toEnum 0) 0), c)) <$> (( + 1) . ( `div` 25) . Map.size <$> current dRows) <@ startEvt
        -- performEvent_ $ liftIO (putStrLn "TickLossy") <$ ticks
        -- display shouldStopD
        ticks <- ((dyn tickerD >>= switchHold never))
        -- ticks <- Reflex.Dom.Core.traceEvent "tick" <$> ((dyn tickerD) >>= switchHold never)
        count <- foldDyn id 25 $ leftmost [ const 25 <$ updated dRows, (+25) <$ ticks ]
        -- display count
        listWithKey (Map.take <$> count <*> dRows) (\k r -> do
          dAttrs <- rowAttrs k
          elDynAttr' "tr" dAttrs $ mapM (\(_, x, _) -> el "td" $ x k r ) cols
          )
      return (mconcat queryEls, bodyRes) 
======= -}
      initRows <- sample $ currentIncremental iRows
      --let qq :: _
      --     qq = fmap (fmap Nothing) $ Map.difference <$> current dRows <@> updated dRows
      -- let updateddRows = (fmap (fmap $ const Nothing) $ Map.difference <$> current dRows <@> updated dRows) <> (fmap Just <$> updatedIncremental dRows)
      bodyRes <- el "tbody" $ do
        let lster ir ur fun = mapMapWithAdjustWithMove fun ir ur
        -- listWithKeyShallowDiff initRows updateddRows $ (\k r er -> do
        lster initRows (updatedIncremental iRows) $ (\k r -> do
        -- listWithKey dRows (\k r -> do
           dAttrs <- rowAttrs k r
           -- rowData <- holdDyn r er
           elDynAttr' "tr" dAttrs $ mapM (\(_, x, _) -> el "td" $ x k (constDyn r)) cols)
      return (mconcat queryEls, undefined) -- bodyRes) 
-- >>>>>>> batched-query-static-render

backsolve1 :: DomBuilder t m => m ()
backsolve1 = elAttr "span" ("class" =: "tooltip" <> "style" =: "font-family: 'SymbolaRegular'" <> "data-tooltip" =: "This solution was backsolved.") $ do
  text " 🝡⃪"

headerDropdownSettings :: Reflex t => DropdownConfig t a
headerDropdownSettings = def & dropdownConfig_attributes .~ (constDyn $ "class" =: "grow shrink w-4/5 max-w-xs min-w-4")


puzzlesTable :: forall js t m. (Template t m, MonadHold t m, MonadFix m, MonadIO (Performable m), TriggerEvent t m, PerformEvent t m, Prerender t m
     , SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m )
  =>
  PuzzleTableConfig t m -> m ()
--  Dynamic t (Map (PrimaryKey Puzzle Identity) (Puzzle Identity)) -> m ()
puzzlesTable PuzzleTableConfig { _puzzleTableConfig_query = query, _puzzleTableConfig_modifyQuery = modifyQuery, _puzzleTableConfig_results = puzzles, _puzzleTableConfig_puzzleLink = puzzleLink, _puzzleTableConfig_metas = knownMetas, _puzzleTableConfig_tags = knownTags } = divClass "top-scrollable" $ mdo
          traceM "Starting puzzle table"
          {-queryEl <- inputElement $ def -- & inputElementConfig_initialValue .~ (T.pack $ show (mempty :: PuzzleQuery))
          el "br" blank
          let queries = fmap fst . (reads :: String -> [(PuzzleQuery, String)]) . T.unpack <$> _inputElement_value queryEl
          el "br" blank
          let query = Prelude.head <$> (queries <> ((:[]) <$> tblquery))
          display query
          el "br" blank -}
          --(tableQueryD :: Dynamic t ([Int]), tableResD) <- 
{- <<<<<<< HEAD
          -- setRoute $ (FrontendRoute_Puzzle :/) . ((,) ((coerce :: Int64->Id Hunt) 2)) . Left <$> updated query
          -- modifyRoute $ id <$ updated query
          (queryNope :: Dynamic t PuzzleQuery, _) <- traceShow "Puzzle list started" $ tableDynAttrWithSearch @(_) "puzzletable ui celled table"
            [ ("Title", \puzKey puzDat -> puzzleLink (primaryKey <$> (_puzzleData_puzzle <$> puzDat)) $ elAttr "div" ("class" =: "" <> "data-tooltip" =: "Open Puzzle") $ dynText $ _puzzle_Title <$> (_puzzleData_puzzle <$> puzDat), return $ (constDyn mempty))
            , ("Is meta?", \_ puzDat -> dynText $ (\p -> if p then "META" else "") . _puzzle_IsMeta <$> (_puzzleData_puzzle <$> puzDat)
              , do
                  startValue <- sample $ current $ ((\a -> fromMaybe mempty $ matchSubSelect a (`elem` ([PuzzleSelect_IsMeta, PuzzleSelect_Not PuzzleSelect_IsMeta] :: [PuzzleSelect]))) . _puzzleQuery_select) <$> query
                  dropdownValue <- fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown startValue (constDyn (mempty =: " - " <> PuzzleSelect_IsMeta =: "Is Meta" <> (PuzzleSelect_Not PuzzleSelect_IsMeta) =: "Not Meta")) headerDropdownSettings
                  modifyQuery $ (\(PuzzleQuery old _) (PuzzleQuery new _) -> Endo $ \(PuzzleQuery all ord) -> PuzzleQuery (new <> subPuzzleSelect all old) ord) <$> current dropdownValue <@> updated dropdownValue
                  pure dropdownValue
              )
            , ("Meta", \_ puzDat -> void $
                listWithKey (_puzzleData_metas <$> puzDat) $ \k dV -> puzzleLink (constDyn k) $ dynText dV
======= -}
          -- jlet query = constDyn mempty
          -- uniqQuery <- holdDyn mempty $ updated query

          -- let puzzleDataDynamic = traceDynWith (\t -> show ("puzzleDataDynamic updated", Map.keys t)) $ (toSortKeys (_puzzleQuery_ordering <$> uniqQuery) $ prunePuzzles (_puzzleQuery_select <$> uniqQuery) $ puzzles)

          -- initialPuzzles <- sample $ current puzzleDataDynamic
--          let puzzleIdsD = imap (\i _ -> i) <$> puzzleDataDynamic
          -- puzzleIncremental <- holdIncremental initialPuzzles $ patchThatChangesMap <$> currentIncremental puzzleIncremental <@> updated puzzleDataDynamic

          
          {-display $ Map.keys <$> puzzleDataDynamic
          display $ join $ (sequenceA . fmap _puzzleData_puzzle) <$> puzzleDataDynamic
          display $ join $ (sequenceA . fmap _puzzleData_metas) <$> puzzleDataDynamic
          -- display $ join $ (sequenceA . fmap _puzzleData_solutions) <$> puzzleDataDynamic
          display $ join $ (sequenceA . fmap _puzzleData_tags) <$> puzzleDataDynamic -}
          -- display $ join $ (sequenceA . fmap _puzzleData_notes) <$> puzzleDataDynamic
          -- Reflex.Dom.Core.traceEvent $ "puzzleDataDynamic updated" <$ updated puzzleDataDynamic
          performEvent_ $ traceM . ("puzzles incremental updated: " <>) . show <$> updatedIncremental puzzles
          switchSignal <- prerender blank blank
          performEvent_ $ traceM "switching to live site" <$ updated switchSignal
          let dumbList :: Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m ()
              dumbList mapD w = dyn_ $ ffor mapD $ \metMap -> forM_ (Map.toList metMap) $ \(k, v) -> w k (constDyn v)
          
          (_, _) <- traceShow "Puzzle list started" $ tableDynAttrWithSearch "puzzletable ui celled table"  [
            -- ("Title", \_ _ _ -> puzzleLink (constDyn text "", pure $ (constDyn mempty))
            ("Title", \_ puzDat -> puzzleLink (primaryKey . _puzzleData_puzzle <$> puzDat) $ elAttr "div" ("class" =: "" <> "data-tooltip" =: "Open Puzzle") $ dynText $ _puzzle_Title . _puzzleData_puzzle <$> puzDat, return $ (constDyn (mempty :: PuzzleQuery)))
            , ("Is meta?", \_ puzDat -> dynText $ (\p -> if p then "META" else "") . _puzzle_IsMeta . _puzzleData_puzzle <$> puzDat
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_IsMeta =: "Is Meta" <> (PuzzleSelect_Not PuzzleSelect_IsMeta) =: "Not Meta")) headerDropdownSettings
              )
            , ("Meta", \_ puzDat -> void $
                -- listWithKey (_puzzleData_metas <$> puzDat) $ \k dV -> puzzleLink (constDyn k) $ dynText $ dV
                dyn_ $ ffor (_puzzleData_metas <$> puzDat) $ \metMap -> forM_ (Map.toList metMap) $ \(metId, met) -> puzzleLink (constDyn metId) $ text met
                , 
                  elClass "span" "flex flex-row" $ do
                    startValue <- sample $ current $ ((\metas a -> fromMaybe mempty $ matchSubSelect (_puzzleQuery_select a) $ (\case { PuzzleSelect_HasMeta _ -> True; _ -> False }))) <$> knownMetas <*> query
                    queryByMeta <- fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown startValue (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_HasMeta <$> knownMetas) headerDropdownSettings
                    startSortValue <- sample $ current $ (\(PuzzleQuery _ ord) -> ord == PuzzleOrdering_ByMeta) <$> query
                    sortByMeta <- elClass "span" "flex-initial max-w-min" $ fmap (PuzzleQuery PuzzleSelect_All . (\a -> if a then PuzzleOrdering_ByMeta else PuzzleOrdering_Any)) <$> semToggle "" startSortValue
                    modifyQuery $ (\(PuzzleQuery old _) (PuzzleQuery new _) -> Endo $ \(PuzzleQuery all ord) -> PuzzleQuery (new <> subPuzzleSelect all old) ord) <$> current queryByMeta <@> updated queryByMeta
                    modifyQuery $ (\(PuzzleQuery _ n) -> Endo $ \(PuzzleQuery all _) -> PuzzleQuery all n) <$> updated sortByMeta
                    pure $ sortByMeta <> queryByMeta
                ) 
            , ("Solution(s)", \_ puzDat -> do
                dyn_ $ ffor (_puzzleData_solutions <$> puzDat) $ \solMap -> forM_ (Map.toList solMap) $ \(solId, sol) -> do
                  el "pre" $ do 
                  text $ _solution_Solution sol
                  if _solution_IsBacksolve sol then backsolve1 else blank
              , do
                   startValue <- sample $ current $ ((\a -> fromMaybe mempty $ matchSubSelect a (`elem` ([PuzzleSelect_HasSolution, PuzzleSelect_Not PuzzleSelect_HasSolution] :: [PuzzleSelect]))) . _puzzleQuery_select) <$> query
                   dropdownValue <- fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown startValue (constDyn (mempty =: " - " <> PuzzleSelect_HasSolution =: "Has Solution" <> (PuzzleSelect_Not PuzzleSelect_HasSolution) =: "No Solution")) headerDropdownSettings
                   modifyQuery $ (\(PuzzleQuery old _) (PuzzleQuery new _) -> Endo $ \(PuzzleQuery all ord) -> PuzzleQuery (new <> subPuzzleSelect all old) ord) <$> current dropdownValue <@> updated dropdownValue
                   pure dropdownValue
              )
            , ("Status", \_ puzDat -> 
                void $ dumbList (Map.filterWithKey (\k _ -> k `elem` statusTags) <$> (_puzzleData_tags <$> puzDat)) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
            , do
                   startValue <- sample $ current $ ((\a -> fromMaybe mempty $ matchSubSelect a (`elem` ((PuzzleSelect_WithTag <$> ["done", "extraction", "in-progress", "solved", "stalled"]) :: [PuzzleSelect]))) . _puzzleQuery_select) <$> query
                   dropdownValue <- fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown startValue (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_WithTag . Map.fromSet (id) <$> constDyn statusTags) headerDropdownSettings
                   modifyQuery $ (\(PuzzleQuery old _) (PuzzleQuery new _) -> Endo $ \(PuzzleQuery all ord) -> PuzzleQuery (new <> subPuzzleSelect all old) ord) <$> current dropdownValue <@> updated dropdownValue
                   pure dropdownValue
              )
            , ("Current Solvers", \_ puzDat -> 
                void $ dumbList (_puzzleData_currentSolvers <$> puzDat) $ \k u -> el "span" $ dynText u
              , do
                  startValue <- sample $ current $ ((\a -> fromMaybe mempty $ matchSubSelect a (`elem` ([PuzzleSelect_HasSolvers, PuzzleSelect_Not PuzzleSelect_HasSolvers] :: [PuzzleSelect]))) . _puzzleQuery_select) <$> query
                  dropdownValue <- fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown startValue (constDyn (mempty =: " - " <> PuzzleSelect_HasSolvers =: "Has Current Solvers" <> (PuzzleSelect_Not PuzzleSelect_HasSolvers) =: "No Current Solvers")) headerDropdownSettings
                  modifyQuery $ (\(PuzzleQuery old _) (PuzzleQuery new _) -> Endo $ \(PuzzleQuery all ord) -> PuzzleQuery (new <> subPuzzleSelect all old) ord) <$> current dropdownValue <@> updated dropdownValue
                  pure dropdownValue
              )
            {- , ("Voice Chat", \_ puzDat _ -> 
                let lnkD = _puzzle_voicelink $ (_puzzleData_puzzle $ puzDat)
=======
            , ("Status", \_ puzDat _ -> 
                void $ listWithKey (Map.filterWithKey (\k _ -> k `elem` statusTags) <$> (_puzzleData_tags puzDat)) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
               
            , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_WithTag . Map.fromSet (id) <$> constDyn statusTags) headerDropdownSettings
              )
            , ("Current Solvers", \_ puzDat _ -> 
                void $ listWithKey (_puzzleData_currentSolvers puzDat) $ \k u -> el "span" $ dynText u
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (constDyn (mempty =: " - " <> PuzzleSelect_HasSolvers =: "Has Current Solvers" <> (PuzzleSelect_Not PuzzleSelect_HasSolvers) =: "No Current Solvers")) headerDropdownSettings
              ) -}
            , ("Voice Chat", \_ puzDat -> 
                let lnkD = _puzzle_voicelink . _puzzleData_puzzle <$> puzDat
                in elDynAttr "a" (fromMaybe mempty . fmap ((<> ("class" =: "text-xs voicelink" <> "target" =: "_blank")) . ("href" =:)) <$> lnkD) $ dynText $ fromMaybe "" . ("Voice Chat" <$) <$> lnkD
              , do
                  startValue <- sample $ current $ ((\a -> fromMaybe mempty $ matchSubSelect a (`elem` ([PuzzleSelect_HasVoice, PuzzleSelect_Not PuzzleSelect_HasVoice] :: [PuzzleSelect]))) . _puzzleQuery_select) <$> query
                  dropdownValue <- fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown startValue (constDyn (mempty =: " - " <> PuzzleSelect_HasVoice =: "Has Voice Chat" <> (PuzzleSelect_Not PuzzleSelect_HasVoice) =: "No Voice Chat")) headerDropdownSettings
                  modifyQuery $ (\(PuzzleQuery old _) (PuzzleQuery new _) -> Endo $ \(PuzzleQuery all ord) -> PuzzleQuery (new <> subPuzzleSelect all old) ord) <$> current dropdownValue <@> updated dropdownValue
                  pure dropdownValue
              )
-- <<<<<<< HEAD
            , ("Tags", \_ puzDat ->
                void $ dumbList (Map.filterWithKey (\k _ -> not $ k `elem` statusTags) <$> (_puzzleData_tags <$> puzDat)) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
                -- void $ listWithKey ((\pd -> _puzzleData_tags pd Map.\\ Map.fromList ((\a -> (a, ())) <$> _puzzleData_status pd)) <$> puzDat) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
              , do
                  startValue <- sample $ current $ ((\a -> fromMaybe mempty $ matchSubSelect a (\case { PuzzleSelect_WithTag _ -> True; _ -> False })) . _puzzleQuery_select) <$> query
                  dropdownValue <- fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown startValue (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_WithTag . Map.fromSet (id) <$> knownTags) headerDropdownSettings
                  modifyQuery $ (\(PuzzleQuery old _) (PuzzleQuery new _) -> Endo $ \(PuzzleQuery all ord) -> PuzzleQuery (new <> subPuzzleSelect all old) ord) <$> current dropdownValue <@> updated dropdownValue
                  pure dropdownValue
              )
            , ("Notes", \_ puzDat ->
                void $ dumbList (_puzzleData_notes <$> puzDat) $ \k dV -> elClass "div" "" $ dynText $ _note_Note <$> dV
{- =======
            , ("Tags", \_ puzDat _ ->
              void $ listWithKey (Map.filterWithKey (\k _ -> not $ k `elem` statusTags) <$> (_puzzleData_tags puzDat)) $ \k _ -> elAttr "span" ("class" =: "ui label" <> "data-tag" =: k) $ text k
              , fmap (flip PuzzleQuery PuzzleOrdering_Any) . _dropdown_value <$> dropdown mempty (( (mempty =: " - ") <>) . Map.mapKeys PuzzleSelect_WithTag . Map.fromSet (id) <$> knownTags) headerDropdownSettings
              )
            , ("Notes", \_ puzDat _ ->
              void $ listWithKey (_puzzleData_notes puzDat) $ \k dV -> elClass "div" "" $ dynText $ _note_Note <$> dV
>>>>>>> batched-query-static-render
-}
              , return $ constDyn mempty)
            ]
            puzzles
            (\k r -> do
	       let hiddenAttr = (\a -> if a then mempty else "hidden" =: "") <$> ((shouldShowPuzzle . _puzzleQuery_select <$> query) <*> constDyn r)
               pure $ hiddenAttr) -- constDyn mempty)
          blank
