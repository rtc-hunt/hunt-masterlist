{-# Language ScopedTypeVariables #-}
{-# Language ImpredicativeTypes #-}

module Templates.Puzzle where

import Data.Default
import Data.Text
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding as T
import Data.Map (Map)
import GHC.Generics (Generic, Generic1)
import Database.Beam (tableLenses)
import Database.Beam.Schema.Tables (Beamable, zipBeamFieldsM, LensFor(..), tblSkeleton, Columnar'(..), Lenses)
import Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Control.Lens
import Reflex.Dom.Core
import Control.Monad.Identity
import Obelisk.Route.Frontend
import Database.Beam

import qualified Data.Aeson as Aeson

import Common.Schema
import Frontend.Types
import Frontend.Utils
import Templates.Types

data PuzzleConfig t m = PuzzleConfig
  { _puzzleConfig_puzzle :: Dynamic t (PuzzleData t)
  , _puzzleConfig_tab :: Dynamic t PuzzlePageTab
  , _puzzleConfig_chatWidget :: m ()
  , _puzzleConfig_configuratorWidget :: m ()
  , _puzzleConfig_puzzleLink :: PrimaryKey Puzzle Identity -> m () -> m ()
  }

data PuzzleOut t m = PuzzleOut
--  { _puzzleTableOut_
--  }

ifJustWidget someD someWidget =
  dyn_ $ ffor someD $ \case
    Just f -> someWidget $ constDyn f
    Nothing -> blank

data PuzzlePageTab
  = PuzzlePageTab_Puzzle
  | PuzzlePageTab_Sheet
  | PuzzlePageTab_Chat
  | PuzzlePageTab_Config
  deriving (Enum, Ord, Bounded, Eq, Show)

puzzleView :: forall t m js. (Template t m, MonadHold t m, MonadFix m, Prerender js t m) => PuzzleConfig t m -> m ()
puzzleView (PuzzleConfig
  { _puzzleConfig_puzzle = puzData
  , _puzzleConfig_tab = puzTabD
  , _puzzleConfig_chatWidget = chatWidget
  , _puzzleConfig_configuratorWidget = configuratorWidget
  , _puzzleConfig_puzzleLink = puzLink
  }) = do
  let frameURI uriDM = divClass "framed" $ elDynAttr "iframe" (("src" =: ) . fromMaybe "" <$> uriDM) blank
  myTabDisplay "ui top attached tabular menu" "activeTab" puzTabD $
    PuzzlePageTab_Puzzle =: ("puzzle", frameURI $ Just . _puzzle_URI <$> (puzData >>= _puzzleData_puzzle))
    <> PuzzlePageTab_Sheet =: ("sheet", frameURI $ _puzzle_SheetURI <$> (puzData >>= _puzzleData_puzzle))
    <> PuzzlePageTab_Chat =: ("chat", chatWidget)
    <> PuzzlePageTab_Config =: ("config", divClass "framed" configuratorWidget)
  blank

data PuzzleConfiguratorConfig t = PuzzleConfiguratorConfig
  { _puzzleConfiguratorConfig_puzzle :: Dynamic t (PuzzleData t)
  , _puzzleConfiguratorConfig_knownTags :: Dynamic t (Set Text)
  , _puzzleConfiguratorConfig_huntMetas :: Dynamic t (Map (Id Puzzle) Text)
  -- , _puzzleConfiguratorConfig_puzzleCfg :: Puzzle (ConfiguratorField t m)
  -- , _puzzleConfiguratorConfig_labeledField :: forall a. Text -> (a -> Text) -> Dynamic t a -> InputEl t m
  }

data PuzzleConfiguratorOut t = PuzzleConfiguratorOut
  { _puzzleConfiguratorOut_puzzle :: Event t (Puzzle Identity)
  , _puzzleConfiguratorOut_addSolution :: Event t (Text, Bool)
  , _puzzleConfiguratorOut_removeSolution :: Event t (Id Solution)
  , _puzzleConfiguratorOut_addMeta :: Event t (Id Puzzle)
  , _puzzleConfiguratorOut_removeMeta :: Event t (Id Metapuzzle)
  , _puzzleConfiguratorOut_addTag :: Event t (Text)
  , _puzzleConfiguratorOut_removeTag :: Event t Text
  , _puzzleConfiguratorOut_addNote :: Event t Text
  }

backsolve1 :: DomBuilder t m => m ()
backsolve1 = elAttr "span" ("class" =: "tooltip" <> "style" =: "font-family: 'SymbolaRegular'" <> "data-tooltip" =: "This solution was backsolved.") $ do
  text " 🝡⃪"

Puzzle _ (LensFor puzzleTitle) (LensFor puzzleUri) (LensFor puzzleSheetURI) (LensFor puzzleIsMeta) _ _ _ _ (LensFor puzzleVoiceLink) = tableLenses

puzzleConfigurator
  :: forall t m js. (Template t m, MonadHold t m, MonadFix m, Prerender js t m)
  => PuzzleConfiguratorConfig t
  -> m (PuzzleConfiguratorOut t)
puzzleConfigurator PuzzleConfiguratorConfig
  { _puzzleConfiguratorConfig_puzzle = puzData
  , _puzzleConfiguratorConfig_knownTags = knownTags
  , _puzzleConfiguratorConfig_huntMetas = metas
  } = do
    divClass "framed" $ divClass "top-scrollable" $ divClass "ui container" $ do
      updatePuzzle <- divClass "ui vertical segment" $ do
          -- elClass "form" "ui form" $ do
              el "h1" $ text "Basic Information"
              let labeledField f v label = ConfiguratorField $ \val -> divClass "field" $ do
                    el "label" $ text label
                    initial <- v <$> sample (current val)
                    ie <- inputElement $ def & inputElementConfig_initialValue .~ initial & inputElementConfig_setValue .~ (updated $ v <$> val)
                    return $ f <$> _inputElement_value ie
              entityConfigurator $ ConfiguratorConfig
                { _configuratorConfig_fields = 
                    def & puzzleTitle .~ labeledField id id "Title"
                        & puzzleUri .~ labeledField id id "Puzzle URI"
                        & puzzleSheetURI .~ labeledField Just (fromMaybe "") "Puzzle Sheet URI"
                        & puzzleIsMeta .~ (ConfiguratorField $ \val -> divClass "field" $ divClass "ui toggle checkbox" $ do
                            initial <- sample (current val)
                            ie <- inputElement $ 
                               def & inputElementConfig_initialChecked .~ initial
                                   & inputElementConfig_setChecked .~ updated val
                                   & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox")
                            el "label" $ text "Is Meta?"
                            return $ _inputElement_checked ie)
                        & puzzleVoiceLink .~ (labeledField (\case { "" -> Nothing; a -> Just a; }) (fromMaybe "") "Voice Channel Link")
                , _configuratorConfig_lenses = tableLenses
                , _configuratorConfig_submit = button "Update"
                , _configuratorConfig_warnchange = text "Warning: the data on the backend was changed."
                , _configuratorConfig_value = puzData >>= _puzzleData_puzzle
                }
      (removeSolution, addSolution) <- divClass "ui vertical segment" $ do
            el "h2" $ text "Solution(s)"
            removals <- listWithKey (puzData >>= _puzzleData_solutions) $ \k sol ->  do
              el "pre" $ do
                dynText $ _solution_Solution <$> sol
                dyn_ $ (\a -> if a then backsolve1 else blank) . _solution_IsBacksolve <$> sol
                buttonClass "ui right floated button" "Remove"
            newSolve <- inputElement $ def
            isBacksolve <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox")
            
            addSolve <- button "Add"
            pure ( switchDyn $ leftmost . Map.elems . imap (\i evt -> i <$ evt) <$> removals
                 , current ((,) <$> _inputElement_value newSolve <*> _inputElement_checked isBacksolve) <@ addSolve)
      (removeMeta, addMeta) <- divClass "ui vertical segment" $ mdo
            el "h2" $ text "Meta(s)"
            divClass "ui large relaxed celled clearing list" $ do
              removeEvts <- listWithKey (puzData >>= _puzzleData_metas) $ \k mn -> elClass "div" "item" $ do
                removeEvt <- buttonClass "ui right floated button" "Remove"
                divClass "header" . dynText $ mn
                pure removeEvt
              addMeta <- dropdown Nothing ((<> Nothing =: "Add Meta") . Map.mapKeys Just <$> metas) $ def
              pure (MetapuzzleId <$> current (puzData >>= fmap primaryKey . _puzzleData_puzzle) <@> (switchDyn $ leftmost . Map.elems . imap (\i evt -> i <$ evt) <$> removeEvts), fmapMaybe id ( _dropdown_change addMeta))
      (removeTagEvents, addTag) <- divClass "ui vertical segment" $ do
            el "h2" $ text "Tags"
            removeTagEvents <- divClass "ui basic segment" $ do
              listWithKey (puzData >>= _puzzleData_tags) $ \tag _ -> elClass "span" "item" $ do
                elAttr "span" ("class" =: "ui left labeled button") $ do
                  divClass "ui large label" $ text $ tag
                  buttonClassIcon "ui tiny icon button" "minus square icon"
            addTag <- dropdown Nothing ((<> Nothing =: "Add Tag") . Map.mapKeys Just . Map.fromSet id <$> knownTags) $ def
            ipt <- inputElement $ def -- & inputElementConfig_ .~ 
            --  & inputElementConfig_setValue .~ ("" <$ e)
            addE <- button "add"

            pure ((switchDyn $ leftmost . Map.elems . imap (\i evt -> i <$ evt) <$> removeTagEvents)
                 , leftmost 
                   [ fmapMaybe id (_dropdown_change addTag)
                   , current (_inputElement_value ipt) <@ addE
                   ])
      newNote <- divClass "ui vertical segment" $ do
            el "h2" $ text "Notes"
            newNote <- inputElement $ def
            listWithKey (puzData >>= _puzzleData_notes) $ \id note -> elClass "div" "border-solid p-4 border-black rounded border-2" $ dynText $ _note_Note <$> note
            addNoteButton <- button "Add Note"
            pure (current (_inputElement_value newNote) <@ addNoteButton)
      return $ PuzzleConfiguratorOut
        { _puzzleConfiguratorOut_puzzle = updatePuzzle
        , _puzzleConfiguratorOut_addSolution = addSolution
        , _puzzleConfiguratorOut_removeSolution = removeSolution
        , _puzzleConfiguratorOut_addMeta=addMeta
        , _puzzleConfiguratorOut_removeMeta = removeMeta
        , _puzzleConfiguratorOut_addTag = addTag
        , _puzzleConfiguratorOut_removeTag = removeTagEvents
        , _puzzleConfiguratorOut_addNote = newNote
        }

-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
myTabDisplay :: forall k t m js. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js t m, Enum k)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ Class applied to currently active <li> element
  -> Dynamic t k 
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
myTabDisplay ulClass activeClass curTab tabItems = do
  let t0 = listToMaybe $ Map.keys tabItems
  let tccAttrs = (\active -> "class" =: "tabbedContentContainer" <> "style" =: ("right: " <> (pack $ show $ active) <> "00%;")) . fromEnum <$> curTab
  elClass "div" "tabbedContent" $ elDynAttr "div" tccAttrs $ do
    iforM_ tabItems $ \k (_, w) -> do
      divClass "" w
    return ()
