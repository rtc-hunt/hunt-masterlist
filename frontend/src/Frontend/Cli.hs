{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{- LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# OPTIONS_GHC -Werror #-}

module Frontend.Cli where

import Reflex
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative as Opt
import Data.Dependent.Sum
-- import Data.Constraint
-- import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.GADT.Compare.TH
import Data.GADT.Show
import Control.Monad.Identity
import qualified Data.Dependent.Map as DMap
import Database.Beam.Backend.SQL.Types (SqlSerial(..))
import Common.Schema
import Common.Request
import Data.Proxy
import Data.Some
import Reflex.Dom.Core hiding (value)
import qualified Data.Set as Set
import Frontend.Types

import Rhyolite.Api (ApiRequest(..))

data CliCommandTag a where
 CliCommandTag_Me :: CliCommandTag Text
 CliCommandTag_PuzzleCommand :: CliCommandTag (Some PuzzleCommand)
 CliCommandTag_Renick :: CliCommandTag Text

instance GShow CliCommandTag where
  gshowsPrec _ CliCommandTag_Me = showString "CliCommandTag_Me"
  gshowsPrec _ CliCommandTag_PuzzleCommand = showString "CliCommandTag_PuzzleCommand"
  gshowsPrec _ CliCommandTag_Renick = showString "CliCommandTag_Renick"

allCliCommands :: [DSum CliCommandTag Proxy]
allCliCommands = [CliCommandTag_Me :=> Proxy, CliCommandTag_PuzzleCommand :=> Proxy, CliCommandTag_Renick :=> Proxy]

cliCommandParser :: Maybe (Id Puzzle) -> ParserInfo (DSum CliCommandTag Identity)
cliCommandParser theRoute = info (hsubparser commands <**> helper) fullDesc
  where
    commands :: Mod CommandFields (DSum CliCommandTag Identity)
    commands = mconcat $ ffor allCliCommands $ \case
      CliCommandTag_Me :=> _ -> 
        command "me" $ 
          info (fmap ((CliCommandTag_Me ==>) . T.intercalate " ") $ 
            some $ fmap T.pack $ strArgument $ metavar "TEXT") $ fullDesc <> noIntersperse
      CliCommandTag_Renick :=> _ ->
        command "nick" $
          info (fmap ((CliCommandTag_Renick ==>) . T.intercalate " ") $ 
            some $ fmap T.pack $ strArgument $ metavar "NAME") $ fullDesc <> noIntersperse
      (CliCommandTag_PuzzleCommand :=> _) -> mconcat
        [ command "status" $ info (
          fmap ((CliCommandTag_PuzzleCommand ==>) . mkSome) $ 
             hsubparser statusCommands
           ) $ fullDesc
        , command "tag" $ info (
          fmap ((CliCommandTag_PuzzleCommand ==>) . mkSome) $
             (PuzzleCommand_Tag <$> puzzleOption <*> (fmap T.pack $ strArgument $ metavar "TAG"))
           ) $ fullDesc
        , command "untag" $ info (
          fmap ((CliCommandTag_PuzzleCommand ==>) . mkSome) $
             (PuzzleCommand_Untag <$> puzzleOption <*> (fmap T.pack $ strArgument $ metavar "TAG"))
           ) $ fullDesc
        , command "note" $ info (
          fmap ((CliCommandTag_PuzzleCommand ==>) . mkSome) $
             (PuzzleCommand_Note <$> puzzleOption <*> fmap (T.intercalate " ") (some (fmap T.pack $ strArgument $ metavar "MULTI-WORD NOTE")))
           ) $ fullDesc
        , command "solve" $ info (
          fmap ((CliCommandTag_PuzzleCommand ==>) . mkSome) $
             ((\p b s -> PuzzleCommand_Solve p s b)
                <$> puzzleOption 
                <*> Opt.switch (long "backsolve" <> short 'b' <> help "Solution was backsolved"))
                <*> fmap (T.intercalate " ") (some (fmap T.pack $ strArgument $ metavar "SOLUTION")) 
           ) $ fullDesc
        , command "delete-puzzle" $ info (
          fmap ((CliCommandTag_PuzzleCommand ==>) . mkSome) $
             (PuzzleCommand_DeletePuzzle <$> puzzleOption )
           ) $ fullDesc
        , command "voice" $ info (
          fmap ((CliCommandTag_PuzzleCommand ==>) . mkSome) $
             (PuzzleCommand_Voice <$> puzzleOption <*> optional (fmap T.pack $ strArgument $ metavar "VOICE_CHAT_URL"))
           ) $ fullDesc
        , command "ht" $ info (
          fmap ((CliCommandTag_PuzzleCommand ==>) . mkSome) $
             (PuzzleCommand_HuntTools <$> puzzleOption <*> fmap (T.intercalate " ") (some (fmap T.pack $ strArgument $ metavar "Hunttools query")))
           ) $ fullDesc
        ]
    puzzleOption :: Parser (PrimaryKey Puzzle Identity)
    puzzleOption = option (PuzzleId . SqlSerial <$> auto) (short 'p' <> long "puzzle" <> metavar "PUZZLE" <> puzzleOptionMod <> help "Puzzle identifier. Auto-fills on puzzle pages, which is the expected use.")
    puzzleOptionMod :: Mod OptionFields (PrimaryKey Puzzle Identity) = case theRoute of
      (Just puz) -> value puz
      _ -> idm
    -- statusCommands :: Mod CommandFields (PuzzleCommand (Id Puzzle, Text))
    statusCommands :: Mod CommandFields (PuzzleCommand (Id Puzzle, Text))
    statusCommands = mconcat $ Set.toList statusTags >>= \theTag -> [
          command (T.unpack theTag) $ info (
             (PuzzleCommand_Tag <$> puzzleOption <*> pure theTag)
           ) $ fullDesc,
          command ("not-" ++ T.unpack theTag) $ info (
             (PuzzleCommand_Untag <$> puzzleOption <*> pure theTag)
           ) $ fullDesc
           ]
      -- (\tag -> flip PuzzleCommand_Tag tag <$> command (T.unpack tag) (info mempty fullDesc)) <$> Set.toList statusTags

cliWords :: Text -> [String]
cliWords a = case words $ T.unpack a of -- Fuggles hack.
  "help":r -> "--help":r
  b -> b

parseCli :: (Reflex t) => Maybe (Id Puzzle) -> Event t Text -> (Event t Text, EventSelector t CliCommandTag)
parseCli routeD iE = fmap (fan . fmap (DMap.fromList . pure)) $ fanEither $ ffor (parseWithRoute routeD <$> iE) $ \case
  Success val -> Right val
  Failure msg -> Left $ T.pack $ fst $ renderFailure msg ""
  CompletionInvoked _ -> Left "oops"
  where
    parseWithRoute route = execParserPure defaultPrefs (cliCommandParser route) . cliWords

requestingSimpleCommands :: (PerformEvent t m, AuthReq t m, Request m ~ ApiRequest () PublicRequest PrivateRequest, MonadHold t m, PostBuild t m, Adjustable t m, NotReady t m)  => EventSelector t CliCommandTag -> m (Event t Text)
requestingSimpleCommands cmdsE = do
  res <- (holdDyn (pure never) $ someReqFor <$> select cmdsE CliCommandTag_PuzzleCommand) >>= dyn
  (res2, _) <- fanEither . fmap runIdentity . Reflex.switch . current <$> holdDyn never res
  requesting_ $ ApiRequest_Private () . PrivateRequest_Renick <$> select cmdsE CliCommandTag_Renick
  
  pure $ res2
  where 
    someReqFor a = withSome a reqFor
    reqFor evt = do
      pb <- getPostBuild
      requesting $ ApiRequest_Private () (PrivateRequest_PuzzleCommand evt) <$ pb

{-
instance ArgDict c PuzzleCommand => ArgDict c CliCommandTag where
  type ConstraintsFor CliCommandTag c = (
    c Text
    , ConstraintsFor PuzzleCommand c
    )
  argDict = \case
    CliCommandTag_Me -> Dict
    CliCommandTag_PuzzleCommand arg -> argDict arg
-}

deriveArgDict ''CliCommandTag
deriveGEq ''CliCommandTag
deriveGCompare ''CliCommandTag
