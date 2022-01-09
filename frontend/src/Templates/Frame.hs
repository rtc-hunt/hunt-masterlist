module Templates.Frame where

import Reflex.Dom.Core
import Reflex
import Control.Monad.Fix
import Data.Text (Text)
import Data.Text as T
import Obelisk.Route.Frontend
import Common.Route

data Framed m t a = Framed
  { _framed_headerItems :: m (a)
  , _framed_body :: a -> Event t Text -> m ()
}

framed :: (Monad m, DomBuilder t m, MonadFix m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, Prerender js t m) => Framed m t a -> m ()
framed Framed
  { _framed_headerItems = header
  , _framed_body = body
  }
  = mdo
    a <- elClass "nav" "app ui fixed inverted menu" $ do
      routeLink (FrontendRoute_Puzzle :/ Nothing) $ divClass "logo header item" $ text "Hunt Master List"
      rv <- header
      divClass "right menu" $ text "Menu"
      return rv
    divClass "appMain" $ divClass "FullTab" $ mdo
      body a cmd
      cmd <- chatInput "Send a message..."
      pure ()
    pure ()

chatInput :: (DomBuilder t m, MonadFix m) => Text -> m (Event t Text)
chatInput placeholder = do
  rec i <- inputElement $ def
        & initialAttributes .~ ("placeholder" =: placeholder <> "class" =: "chatInput" <> "tabindex" =: "1")
        & inputElementConfig_setValue .~ ("" <$ e)
      let e = leftmost [keypress Enter i]
  let v  = current $ value i
  return $ gate (not . T.null <$> v) $ v <@ e
