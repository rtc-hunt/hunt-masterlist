module Templates.Partials.Inputs where

import Reflex.Dom.Core
import Templates.Types

messageInput :: Template t m => Event t () -> m (InputEl t m)
messageInput clearEvent = inputElement $ def
  & initialAttributes .~
    ( "class" =: "focus:outline-none mx-1 font-facit font-label text-label placeholder-light px-3.5 bg-inset rounded shadow-input flex-grow"
      <> "placeholder" =: "Type your message"
      <> "type" =: "text"
    )
  & inputElementConfig_setValue .~ ("" <$ clearEvent)


