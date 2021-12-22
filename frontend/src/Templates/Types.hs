module Templates.Types
  ( module Templates.Types
  , module X
  ) where

import Reflex.Dom.Core as X hiding (El)

type Template t m = (DomBuilder t m, PostBuild t m)

type InputEl t m = InputElement EventResult (DomBuilderSpace m) t

type El t m = Element EventResult (DomBuilderSpace m) t
