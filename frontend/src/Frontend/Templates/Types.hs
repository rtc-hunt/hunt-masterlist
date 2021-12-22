module Frontend.Templates.Types where

import Reflex.Dom.Core

type Template t m = (DomBuilder t m, PostBuild t m)

type InputEl t m = InputElement EventResult (DomBuilderSpace m) t

type El t m = Element EventResult (DomBuilderSpace m) t
