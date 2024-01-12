{-# Language ScopedTypeVariables #-}
{-# Language DeriveAnyClass #-}
{-# Language OverloadedStrings #-}
module Common.Route where

import Control.Category
import Control.Monad.Except
import Control.Lens
import Data.Coerce
import Data.Functor.Identity
import Data.Int
import Database.Beam.Backend.SQL.Types
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route
import Obelisk.Route.TH
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding ((.), id)

import Common.Schema

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.
  BackendRoute_Listen :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Auth :: FrontendRoute (R AuthRoute)
  FrontendRoute_Channel :: FrontendRoute (Maybe (Id Chatroom))
  FrontendRoute_Puzzle :: FrontendRoute (Id Hunt, Either (PuzzleQuery) (Id Puzzle))
  FrontendRoute_HuntSelection :: FrontendRoute ()
  FrontendRoute_Templates :: FrontendRoute (R TemplateRoute)
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data AuthRoute a where
  AuthRoute_Login :: AuthRoute ()
  AuthRoute_Signup :: AuthRoute ()

data PuzzleQuery = PuzzleQuery 
  { _puzzleQuery_select :: PuzzleSelect
  , _puzzleQuery_ordering :: PuzzleOrdering
  }
  deriving (Eq, Show, Ord, Read)

instance Semigroup PuzzleQuery where
  PuzzleQuery sa oa <> PuzzleQuery sb ob = PuzzleQuery (sa <> sb) (oa <> ob)

instance Monoid PuzzleQuery where
  mempty = PuzzleQuery PuzzleSelect_All PuzzleOrdering_Any

puzzleQueryStringOrPuzzle_prism :: Prism' Text (Either PuzzleQuery (Id Puzzle))
puzzleQueryStringOrPuzzle_prism = prism' fromInput toQuery
  where
    fromInput (Left pq) = fromQuery pq
    fromInput (Right ii) = T.pack . show . (coerce :: PrimaryKey Puzzle Identity -> Int64) $ ii
    fromQuery PuzzleQuery
      { _puzzleQuery_select = sel
      , _puzzleQuery_ordering = ord } = toText_sel sel <> toText_ord ord

    toText_sel PuzzleSelect_All = ""
    toText_sel (PuzzleSelect_And a b) = toText_sel a <> " " <> toText_sel b
    toText_sel (PuzzleSelect_Not a) = "not(" <> toText_sel a <> ")"
    toText_sel (PuzzleSelect_WithTag t) = "tag(" <> t <> ")"
    toText_sel PuzzleSelect_HasVoice = "hasVoice"
    toText_sel PuzzleSelect_IsMeta = "isMeta"
    toText_sel PuzzleSelect_HasSolution = "hasSolution"
    toText_sel PuzzleSelect_HasSolvers = "hasSolvers"
    toText_sel (PuzzleSelect_HasMeta meta) = "hasMeta(" <> (T.pack $ show $ (coerce :: PrimaryKey Puzzle Identity -> Int64) meta) <> ")"

    toText_ord PuzzleOrdering_Any = "unordered"
    toText_ord PuzzleOrdering_ByMeta = ""

    toQuery = parseMaybe puzzleOrIdQueryParser
    puzzleOrIdQueryParser :: Parsec () Text (Either PuzzleQuery (Id Puzzle))
    puzzleOrIdQueryParser = (Right . (coerce :: Int64 -> PrimaryKey Puzzle Identity) . read <$> some digitChar) <|> Left <$> puzzleQueryParser
    puzzleQueryParser :: Parsec () Text PuzzleQuery
    puzzleQueryParser = PuzzleQuery <$> (mconcat <$> many (puzzleSelParser >>= (\a -> space >> pure a))) <*> puzzleOrdParser
    puzzleSelParser = choice
      [ PuzzleSelect_HasVoice <$ string "hasVoice"
      , PuzzleSelect_IsMeta <$ string "isMeta"
      , PuzzleSelect_HasSolution <$ string "hasSolution"
      , PuzzleSelect_HasSolvers <$ string "hasSolvers"
      , PuzzleSelect_HasMeta . (coerce :: Int64 -> PrimaryKey Puzzle Identity) . read <$> (string "hasMeta(" >> someTill digitChar (string ")"))
      , PuzzleSelect_WithTag . T.pack <$> (string "tag(" >> someTill printChar (string ")"))
      , PuzzleSelect_Not . mconcat <$> (string "not(" >> someTill puzzleSelParser (string ")"))
      ]
    puzzleOrdParser = choice [ PuzzleOrdering_Any <$ string "unordered", pure PuzzleOrdering_ByMeta ]

data PuzzleSelect
  = PuzzleSelect_All
  | PuzzleSelect_And PuzzleSelect PuzzleSelect
  | PuzzleSelect_Not PuzzleSelect
  | PuzzleSelect_WithTag Text
  | PuzzleSelect_HasVoice
  | PuzzleSelect_IsMeta
  | PuzzleSelect_HasSolution
  | PuzzleSelect_HasSolvers
  | PuzzleSelect_HasMeta (Id Puzzle)
  deriving (Eq, Show, Ord, Read)

instance Semigroup PuzzleSelect where
  PuzzleSelect_All <> PuzzleSelect_All = PuzzleSelect_All
  PuzzleSelect_All <> a = a
  a <> PuzzleSelect_All = a
  a <> b = PuzzleSelect_And a b

instance Monoid PuzzleSelect where
  mempty = PuzzleSelect_All

data PuzzleOrdering
  = PuzzleOrdering_Any
  | PuzzleOrdering_ByMeta
  deriving (Eq, Show, Ord, Read)

instance Semigroup PuzzleOrdering where
  PuzzleOrdering_Any <> PuzzleOrdering_Any = PuzzleOrdering_Any
  a <> PuzzleOrdering_Any = a
  PuzzleOrdering_Any <> a = a

instance Monoid PuzzleOrdering where
  mempty = PuzzleOrdering_Any

data PuzzleSortKey
  = PuzzleSortKey_Id (Id Puzzle)
  | PuzzleSortKey_Synthetic Int
  deriving (Eq, Show, Ord)
data TemplateRoute :: * -> * where
  -- The index route is where we select the template to view
  TemplateRoute_Index :: TemplateRoute ()
  -- The following routes should correspond to the non-template frontend routes
  -- (i.e., pages that users are expected to visit)
  TemplateRoute_Login :: TemplateRoute ()
  TemplateRoute_Signup :: TemplateRoute ()
  TemplateRoute_Channel :: TemplateRoute ()
  TemplateRoute_Main :: TemplateRoute ()
  TemplateRoute_PuzzleList :: TemplateRoute ()
  TemplateRoute_Puzzle :: TemplateRoute ()




subPuzzleSelect q c = case q of
  a | a == c -> PuzzleSelect_All
  (PuzzleSelect_And a b) | a == c -> b
  (PuzzleSelect_And a b) | b == c -> a
  (PuzzleSelect_And a b) -> PuzzleSelect_And (subPuzzleSelect a c) (subPuzzleSelect b c)
  a -> a

matchSubSelect :: PuzzleSelect -> (PuzzleSelect -> Bool) -> Maybe PuzzleSelect
matchSubSelect q m = case q of
  a | m a -> Just a
  (PuzzleSelect_And a b) | m a -> Just a
  (PuzzleSelect_And a b) | m b -> Just b
  (PuzzleSelect_And a b) -> (matchSubSelect a m) <> (matchSubSelect b m)
  _ -> Nothing

{- unsafeShadowEncoder
  :: (MonadError Text check
     , Show a
     , Show b
     , Show c
     , check
     )
  => Encoder check parse a c
  -> Encoder check parse b c
  -> Encoder check parse (Either a b) c
unsafeShadowEncoder f g = Encoder $ do
  vf <- unEncoder f
  vg <- unEncoder g
  pure $ EncoderImpl
    { _encoderImpl_encode = \case
        Left a -> _encoderImpl_encode vf a
        Right b -> _encoderImpl_encode vg b
    , _encoderImpl_decode = \c -> (Left <$> _encoderImpl decode vf c) `catchError` \_ -> Right <$> _encoderImpl_decode vg c
    }
-}

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Auth -> PathSegment "auth" $ pathComponentEncoder $ \case
        AuthRoute_Login -> PathSegment "login" $ unitEncoder mempty
        AuthRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_HuntSelection -> PathSegment "selecthunt" $ unitEncoder mempty
      FrontendRoute_Puzzle -> PathSegment "hunt" $ pathParamEncoder idEncoder $ singlePathSegmentEncoder . reviewEncoder puzzleQueryStringOrPuzzle_prism  -- (readShowEncoder) -- (singlePathSegmentEncoder . idEncoder)
      FrontendRoute_Channel -> PathSegment "channel" $ maybeEncoder (unitEncoder mempty) (singlePathSegmentEncoder . idEncoder)
      FrontendRoute_Templates -> PathSegment "templates" $ pathComponentEncoder $ \case 
        TemplateRoute_Index -> PathEnd $ unitEncoder mempty
        TemplateRoute_Login -> PathSegment "login" $ unitEncoder mempty
        TemplateRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
        TemplateRoute_Channel -> PathSegment "channel" $ unitEncoder mempty
        TemplateRoute_Main -> PathSegment "main" $ unitEncoder mempty
        TemplateRoute_PuzzleList -> PathSegment "puzzlelist" $ unitEncoder mempty
        TemplateRoute_Puzzle -> PathSegment "puzzle" $ unitEncoder mempty)

checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error $ show e
  Right x -> x

idEncoder :: forall x check parse.
  ( MonadError Text parse
  , Applicative check
  , Coercible (PrimaryKey x Identity) (SqlSerial Int64)
  )
  => Encoder check parse (PrimaryKey x Identity) Text
idEncoder = unsafeMkEncoder EncoderImpl
  { _encoderImpl_encode = T.pack . show . (coerce :: PrimaryKey x Identity -> Int64)
  , _encoderImpl_decode = \x ->
    (coerce :: Int64 -> PrimaryKey x Identity) <$> tryDecode unsafeTshowEncoder x
  }

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''TemplateRoute
  , ''AuthRoute
  ]
