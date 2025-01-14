{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Frontend.ViewCache where
import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM (currentDocumentUnchecked, currentDocument)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.Node (Node)
import GHCJS.DOM.Element (getInnerHTML)
import GHCJS.DOM.NodeList (IsNodeList, item, getLength)
import GHCJS.DOM.ParentNode (querySelectorAll)
import qualified Data.ByteString as BS
import Common.View
import Data.Map (Map)
import Control.Monad.Trans.Maybe
import Data.Witherable
import Data.Semigroup
import Data.Aeson as A
import Data.Text.Encoding as T
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.Either (fromRight)
import qualified Data.ByteString.Base64 as B64

import Debug.Trace
import Data.IORef
import System.IO.Unsafe

cachedResults :: IORef (Maybe (Map String MyQueryResultType))
cachedResults = unsafePerformIO $ newIORef Nothing

getCachedViewsInner
  :: (Monad m, MonadJSM m, MonadIO m) => m (Maybe (Map String MyQueryResultType))
getCachedViewsInner = do
    stored <- liftIO $ readIORef cachedResults
    case stored of
      Just a -> pure $ Just a
      Nothing ->
          runMaybeT $ do
                doc <- MaybeT currentDocument
                body <- MaybeT $ getBody doc
                nodes :: [Node] <- nodeListNodes =<< querySelectorAll body ("[data-rhyolite-saved-views]" :: JSString)
                payloads :: [Map String MyQueryResultType] <- sequence $ flip fmap nodes $ \node -> do
                    castTo HTMLElement node >>= \case
                      Nothing -> pure mempty
                      Just htmlE -> do
                           theData <- getInnerHTML htmlE
                           let decoded = (fromRight Nothing $ fmap Just $ B64.decode $ T.encodeUtf8 theData) >>= A.decodeStrict
                           MaybeT $ pure $ decoded
                case payloads of
                  _: _ -> liftIO $ atomicWriteIORef cachedResults $ Just $ mconcat payloads
                  [] -> pure ()
                pure $ mconcat payloads


getCachedViews
  :: (Monad m 
#ifdef ghcjs_HOST_OS
     , MonadIO m
#endif
     )
  => m (Maybe (Map String MyQueryResultType))
getCachedViews = do
#ifdef ghcjs_HOST_OS
          getCachedViewsInner
#else
          pure Nothing
#endif

-- | Collect all nodes in the node list.
--
-- TODO: this and the version in exe-config/ghcjs/lookup should be
-- upstreamed to jsaddle.
nodeListNodes :: (IsNodeList l, MonadJSM m) => l -> m [Node]
nodeListNodes es = do
  len <- getLength es
  -- Warning! len is unsigned. If the NodeList is empty, we must avoid
  -- accidentally traversing over [0..maxBound::Word]
  nodes <- traverse (item es) $ if len == 0 then [] else [0..len-1]
  pure $ catMaybes nodes

