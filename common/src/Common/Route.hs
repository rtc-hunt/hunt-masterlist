{-# Language ScopedTypeVariables #-}
module Common.Route where

import Control.Category
import Control.Monad.Except
import Data.Coerce
import Data.Functor.Identity
import Data.Int
import Database.Beam.Backend.SQL.Types
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route
import Obelisk.Route.TH
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
  FrontendRoute_Puzzle :: FrontendRoute (Id Hunt, Maybe (Id Puzzle))
  FrontendRoute_HuntSelection :: FrontendRoute ()
  FrontendRoute_Templates :: FrontendRoute (R TemplateRoute)
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data AuthRoute a where
  AuthRoute_Login :: AuthRoute ()
  AuthRoute_Signup :: AuthRoute ()

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
      FrontendRoute_Puzzle -> PathSegment "hunt" $ pathParamEncoder idEncoder $ maybeEncoder (unitEncoder mempty) (singlePathSegmentEncoder . idEncoder)
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
