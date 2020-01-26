{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Common.App where

import Data.Aeson
import Data.Align
import Data.Map.Monoidal
import qualified Data.Map.Monoidal as MMap
import Data.Text (Text)
import Data.Semigroup (First)
import Data.Witherable
import GHC.Generics
import Reflex hiding (Request)
import Rhyolite.App
import Database.Id.Class
import Rhyolite.SemiMap
import Data.Constraint.Extras.TH
import Data.Aeson.GADT.TH
import Common.Schema

type HMLResponse r = Either HMLError r

data HMLError = HMLError
  deriving (Eq, Ord, Generic, FromJSON, ToJSON)

instance Show HMLError where
  show = undefined

data HMLView a = HMLView
  { _hmlView_echo :: MonoidalMap Text (a, First (Maybe Text))
  , _hmlView_puzzle :: MonoidalMap PuzzleQuery (a, SemiMap (Id Puzzle) Puzzle)
  , _hmlView_solve :: MonoidalMap SolveQuery (a, SemiMap (Id Solve) Solve)
  }
  deriving (Eq, Show, Functor, Foldable, Generic)

instance Semigroup a => Monoid (HMLView a) where
  mempty = HMLView mempty mempty mempty

instance Semigroup a => Semigroup (HMLView a) where
  (<>) a b = HMLView
    { _hmlView_echo = _hmlView_echo a <> _hmlView_echo b
    , _hmlView_puzzle = _hmlView_puzzle a <> _hmlView_puzzle b
    , _hmlView_solve = _hmlView_solve a <> _hmlView_solve b
    }

instance Filterable HMLView where
  mapMaybe f a = HMLView
    { _hmlView_echo = fmapMaybeFst f $ _hmlView_echo a
    , _hmlView_puzzle = fmapMaybeFst f $ _hmlView_puzzle a
    , _hmlView_solve = fmapMaybeFst f $ _hmlView_solve a
    }


data PuzzleQuery = PuzzleQuery_AllPuzzles
                 | PuzzleQuery_Puzzle (Id Puzzle)
  deriving (Eq, Ord, Show, Read, Generic)
instance ToJSON PuzzleQuery
instance FromJSON PuzzleQuery
instance ToJSONKey PuzzleQuery
instance FromJSONKey PuzzleQuery

data SolveQuery = SolveQuery_byPuzzle (Id Puzzle)
  deriving (Eq, Ord, Show, Read, Generic)
instance ToJSON SolveQuery
instance FromJSON SolveQuery
instance ToJSONKey SolveQuery
instance FromJSONKey SolveQuery

data HMLViewSelector a = HMLViewSelector
  { _hmlViewSelector_echo :: MonoidalMap Text a
  , _hmlViewSelector_puzzle :: MonoidalMap PuzzleQuery a
  , _hmlViewSelector_solve :: MonoidalMap SolveQuery a
  }
  deriving (Eq, Show, Functor, Foldable, Generic, Traversable, Additive)

instance Semialign HMLViewSelector where
  alignWith f a b = HMLViewSelector
    { _hmlViewSelector_echo = alignWith f (_hmlViewSelector_echo a) (_hmlViewSelector_echo b)
    , _hmlViewSelector_puzzle = alignWith f (_hmlViewSelector_puzzle a) (_hmlViewSelector_puzzle b)
    , _hmlViewSelector_solve = alignWith f (_hmlViewSelector_solve a) (_hmlViewSelector_solve b)
    }
  zipWith f a b = HMLViewSelector
    { _hmlViewSelector_echo = Data.Align.zipWith f (_hmlViewSelector_echo a) (_hmlViewSelector_echo b)
    , _hmlViewSelector_puzzle = Data.Align.zipWith f (_hmlViewSelector_puzzle a) (_hmlViewSelector_puzzle b)
    , _hmlViewSelector_solve = Data.Align.zipWith f (_hmlViewSelector_solve a) (_hmlViewSelector_solve b)
    }

instance Align HMLViewSelector where
  nil = HMLViewSelector MMap.empty MMap.empty MMap.empty

instance Semigroup a => Semigroup (HMLViewSelector a) where
  (<>) = salign

instance Semigroup a => Monoid (HMLViewSelector a) where
  mempty = nil

instance Filterable HMLViewSelector where
  mapMaybe f a = HMLViewSelector
    { _hmlViewSelector_echo = fmapMaybe f $ _hmlViewSelector_echo a
    , _hmlViewSelector_puzzle = fmapMaybe f $ _hmlViewSelector_puzzle a
    , _hmlViewSelector_solve = fmapMaybe f $ _hmlViewSelector_solve a
    }

instance Group a => Group (HMLViewSelector a) where
  negateG = fmap negateG

instance (Num a, Ord a, Filterable q, Eq (q a), Semigroup (q a)) => PositivePart (HMLViewSelector (q a)) where
  positivePart x =
    let u = fmap (fmapMaybe (\n -> if n > 0 then Just n else Nothing)) x
    in if u == mempty then Nothing else Just u

instance (Semigroup a) => Query (HMLViewSelector a) where
  type QueryResult (HMLViewSelector a) = HMLView a
  crop vs v =
      HMLView
        { _hmlView_echo = MMap.intersectionWith const (_hmlView_echo v) (_hmlViewSelector_echo vs)
        , _hmlView_puzzle = MMap.intersectionWith const (_hmlView_puzzle v) (_hmlViewSelector_puzzle vs)
        , _hmlView_solve = MMap.intersectionWith const (_hmlView_solve v) (_hmlViewSelector_solve vs)
        }

instance ToJSON a => ToJSON (HMLView a)
instance FromJSON a => FromJSON (HMLView a)
instance ToJSON a => ToJSON (HMLViewSelector a)
instance FromJSON a => FromJSON (HMLViewSelector a)

data HMLRequest a where
  HMLRequest_None :: HMLRequest ()

deriveArgDict ''HMLRequest
deriveJSONGADT ''HMLRequest

