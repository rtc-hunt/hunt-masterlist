module Templates.Partials.Message where

import Common.View (Msg(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Templates.Types

message
  :: Template t m
  => Dynamic t Msg
  -> m ()
message mView = do
  elClass "li" "font-facit text-copy flex flex-col mb-6 ml-16 md:ml-0 md:items-end" $ do
    elClass "div" "flex flex-row items-baseline justify-between" $ do
      elClass "div" "text-label md:mr-4" $ dynText (fmap _msg_handle mView)
      elClass "div" "font-bold text-label text-light" $ dynText $ timestamp . _msg_timestamp <$> mView
    elClass "div" "p-4 rounded border border-metaline bg-white w-auto" $ dynText (fmap _msg_text mView)

timestamp :: UTCTime -> Text
timestamp = T.pack . formatTime defaultTimeLocale "%R"
