{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Lists where

import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

data ListItemConfig = ListItemConfig
  { _listItemConfig_clickable :: Bool
  , _listItemConfig_subtext :: Maybe Text
  }

defListItemConfig :: ListItemConfig
defListItemConfig = ListItemConfig
  { _listItemConfig_clickable = False
  , _listItemConfig_subtext = Nothing
  }

instance Default ListItemConfig where
  def = defListItemConfig

-- TODO(skylar): Config?
listItem
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => ListItemConfig
  -> Dynamic t Text
  -> m (Event t ())
listItem cfg label = do
  let topClass = T.intercalate " " $
        [ "flex flex-col py-2 border-b border-metaline"
        ] <> case _listItemConfig_clickable cfg of
          True -> ["cursor-pointer"]
          False -> []
  (e, _) <- elClass' "div" topClass $ do
    elClass "div" "leading-none font-facit text-body text-copy" $ dynText label
    case _listItemConfig_subtext cfg of
      Just subtext -> elClass "div" "mt-1 leading-none font-facit text-label text-light" $ text subtext
      Nothing -> blank
  case _listItemConfig_clickable cfg of
    True -> pure $ domEvent Click e
    False -> pure never
