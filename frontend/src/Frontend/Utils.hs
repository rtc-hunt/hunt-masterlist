{-# LANGUAGE OverloadedStrings #-}

module Frontend.Utils where

import Reflex.Dom.Core

-- TODO move to reflex-dom-core and maybe explain why/when this is necessary
onRender :: (Prerender js t m, Monad m) => m (Event t ())
onRender = fmap updated (prerender blank blank)


