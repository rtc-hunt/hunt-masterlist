{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Backend.Schema where

import Common.Schema
import Rhyolite.Backend.Schema.TH
import Database.Groundhog.TH

mkRhyolitePersist (Just "migrateSchema") [groundhog|
  - entity: Puzzle
  - entity: Solve
  - entity: PuzzleMeta
  - entity: OpenTime
  - entity: PuzzleTag
  - entity: PuzzleNote
  |]

fmap concat $ mapM (uncurry makeDefaultKeyIdInt64)
  [ (''Puzzle, 'PuzzleKey)
  , (''Solve, 'SolveKey)
  , (''PuzzleMeta, 'PuzzleMetaKey)
  , (''OpenTime, 'OpenTimeKey)
  , (''PuzzleTag, 'PuzzleTagKey)
  , (''PuzzleNote, 'PuzzleNoteKey)
  ]
