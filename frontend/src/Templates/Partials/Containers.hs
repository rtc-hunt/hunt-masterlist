module Templates.Partials.Containers where

import Reflex.Dom.Core

screenContainer :: (DomBuilder t m) => m a -> m a
screenContainer = elClass "div" "w-screen h-screen bg-background flex flex-col overflow-hidden"
