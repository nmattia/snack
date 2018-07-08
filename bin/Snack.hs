{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, (.:), (.:?))
import Data.FileEmbed (embedStringFile)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.String.Interpolate
import Shelly (Sh)
import System.Posix.Process (executeFile)
import UnliftIO.Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Options.Applicative as Opts
import qualified Shelly as S

data Mode
  = Standalone SnackNix -- Reads a snack.nix file
  | HPack PackageYaml

-- | Like a FilePath, but Nix friendly
newtype SnackNix = SnackNix { unSnackNix :: FilePath }

newtype PackageYaml = PackageYaml { unPackageYaml :: FilePath }

mkSnackNix :: FilePath -> SnackNix
mkSnackNix = SnackNix -- XXX: this is not nix friendly, but it's ok, because
  -- it'll be gone soon
  --
mkPackageYaml :: FilePath -> PackageYaml
mkPackageYaml = PackageYaml -- XXX: this is not nix friendly, but it's ok, because
  -- it'll be gone soon

data Command
  = Build
  | Run
  | Ghci

main :: IO ()
main = do
    opts <- Opts.execParser (Opts.info (options <**> Opts.helper) mempty)
    runCommand (mode opts) (command opts)

data Options = Options
  { mode :: Mode
  , command :: Command
  }

parseMode :: Opts.Parser Mode
parseMode =
    ((Standalone . mkSnackNix) <$>
        Opts.strOption
        (Opts.long "snack-nix"
        <> Opts.short 's'
        <> Opts.value "./snack.nix"
        <> Opts.metavar "PATH")
        )
    <|>
    ((HPack . mkPackageYaml) <$>
        Opts.strOption
        (Opts.long "package-yaml"
        <> Opts.value "./package.yaml"
        <> Opts.short 'p'
        <> Opts.metavar "PATH")
        )

options :: Opts.Parser Options
options = Options <$> parseMode <*> parseCommand

newtype ModuleName = ModuleName { unModuleName :: T.Text }
  deriving newtype (Ord, Eq, Aeson.FromJSONKey)
  deriving stock Show

data BuildResult
  = BuiltLibrary LibraryBuild
  | BuiltExecutable ExecutableBuild
  | BuiltMulti MultiBuild
  | BuiltGhci GhciBuild
  deriving Show

instance Aeson.FromJSON BuildResult where
    parseJSON v =
      BuiltLibrary <$> (guardBuildType "library" v)
      <|> BuiltExecutable <$> (guardBuildType "executable" v)
      <|> BuiltMulti <$> (guardBuildType "multi" v)
      <|> BuiltGhci <$> (guardBuildType "ghci" v)
      where
        guardBuildType :: FromJSON a => T.Text -> Aeson.Value -> Aeson.Parser a
        guardBuildType ty = Aeson.withObject "build result" $ \o -> do
          bty <- o .: "build_type"
          guard (bty == ty)
          Aeson.parseJSON =<< o .: "result"

newtype GhciBuild = GhciBuild
  { ghciExePath :: NixPath
  }
    deriving stock Show

instance FromJSON GhciBuild where
  parseJSON = Aeson.withObject "ghci build" $ \o ->
    GhciBuild <$> o .: "ghci_path"

-- The kinds of builds: library, executable, or a mix of both (currently only
-- for HPack)
newtype LibraryBuild = LibraryBuild
  { unLibraryBuild :: Map.Map ModuleName NixPath }
  deriving newtype FromJSON
  deriving stock Show

newtype ExecutableBuild = ExecutableBuild
  { exePath :: NixPath }
  deriving stock Show

instance FromJSON ExecutableBuild where
  parseJSON = Aeson.withObject "executable build" $ \o ->
    ExecutableBuild <$> o .: "exe_path"

data MultiBuild = MultiBuild
  { librayBuild :: Maybe LibraryBuild
  , executableBuilds :: Map.Map T.Text ExecutableBuild
  }
  deriving stock Show

instance Aeson.FromJSON MultiBuild where
  parseJSON = Aeson.withObject "multi build" $ \o ->
    MultiBuild
      <$> o .:? "library"
      <*> o .: "executables"

data NixArg = NixArg
  { argType :: NixArgType
  , argName :: T.Text
  , argValue :: T.Text
  }

data NixArgType
  = ArgStr
  | Arg

newtype NixExpr = NixExpr { unNixExpr :: T.Text }

newtype NixPath = NixPath { unNixPath :: T.Text }
  deriving newtype FromJSON
  deriving stock Show

decodeOrFail :: FromJSON a => BS.ByteString -> Sh a
decodeOrFail bs = case Aeson.decodeStrict' bs of
    Just foo -> pure foo
    Nothing -> throwIO $ userError $ unlines
      [ "could not decode " <> show bs ]

nixBuild :: [NixArg] -> NixExpr -> Sh NixPath
nixBuild extraNixArgs nixExpr =
    NixPath <$> runStdin1
      (T.pack [i|
        { #{ intercalate "," funArgs } }:
        let
          spec = builtins.fromJSON specJson;
          pkgs = import (builtins.fetchTarball
            { url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
              sha256 = spec.sha256;
            }) {} ;
          libDir =
            let
              b64 = pkgs.writeTextFile { name = "lib-b64"; text = lib64; };
            in
              pkgs.runCommand "snack-lib" {}
              ''
                cat ${b64} | base64 --decode > out.tar.gz
                mkdir -p $out
                tar -C $out -xzf out.tar.gz
                chmod +w $out
              '';
          snack = pkgs.callPackage libDir {};
        in #{ T.unpack $ unNixExpr $ nixExpr }
      |])
      "nix-build"
      cliArgs
  where
    cliArgs :: [T.Text]
    cliArgs =
      [ "-" -- read expression from stdin
      , "--no-out-link" -- no need for roots
      ] <> (concatMap toCliArgs nixArgs)
    funArgs :: [String]
    funArgs = toFunArg <$> nixArgs
    nixArgs :: [NixArg]
    nixArgs =
      [ NixArg { argType = ArgStr , argName = "specJson", argValue = specJson }
      , NixArg { argType = ArgStr , argName = "lib64", argValue = libb64 }
      ] <> extraNixArgs
    toFunArg :: NixArg -> String
    toFunArg = T.unpack . argName
    toCliArgs :: NixArg -> [T.Text]
    toCliArgs narg = case argType narg of
      { Arg -> "--arg"; ArgStr -> "--argstr" }
      : [ argName narg , argValue narg ]

snackBuild :: SnackNix -> Sh BuildResult
snackBuild snackNix = do
    NixPath out <- nixBuild
      [ NixArg
          { argName = "snackNix"
          , argValue = T.pack $ unSnackNix snackNix
          , argType = Arg
          }
      ]
      $ NixExpr "snack.inferSnackBuild snackNix"
    decodeOrFail =<< liftIO (BS.readFile $ T.unpack out)

snackGhci :: SnackNix -> Sh GhciBuild
snackGhci snackNix = do
    NixPath out <- nixBuild
      [ NixArg
          { argName = "snackNix"
          , argValue = T.pack $ unSnackNix snackNix
          , argType = Arg
          }
      ]
      $ NixExpr "snack.inferSnackGhci snackNix"
    liftIO (BS.readFile (T.unpack out)) >>= decodeOrFail >>= \case
      BuiltGhci g -> pure g
      b -> throwIO $ userError $ "Expected GHCi build, got " <> show b

snackBuildHPack :: PackageYaml -> Sh BuildResult
snackBuildHPack packageYaml = do
    NixPath out <- nixBuild
      [ NixArg
          { argName = "packageYaml"
          , argValue = T.pack $ unPackageYaml packageYaml
          , argType = Arg
          }
      ]
      $ NixExpr "snack.inferHPackBuild packageYaml"
    decodeOrFail =<< liftIO (BS.readFile (T.unpack out))

snackGhciHPack :: PackageYaml -> Sh GhciBuild
snackGhciHPack packageYaml = do
    NixPath out <- nixBuild
      [ NixArg
          { argName = "packageYaml"
          , argValue = T.pack $ unPackageYaml packageYaml
          , argType = Arg
          }
      ]
      $ NixExpr "snack.inferHPackGhci packageYaml"
    liftIO (BS.readFile (T.unpack out)) >>= decodeOrFail >>= \case
      BuiltGhci g -> pure g
      b -> throwIO $ userError $ "Expected GHCi build, got " <> show b

runCommand :: Mode -> Command -> IO ()
runCommand (Standalone snackNix) = \case
  Build -> S.shelly $ void $ snackBuild snackNix
  Run -> quiet (snackBuild snackNix) >>= runBuildResult
  Ghci -> runExe =<< ghciExePath <$> (quiet $ snackGhci snackNix)
runCommand (HPack packageYaml) = \case
  Build -> S.shelly $ void $ snackBuildHPack packageYaml
  Run -> quiet (snackBuildHPack packageYaml) >>= runBuildResult
  Ghci -> runExe =<< ghciExePath <$> (quiet $ snackGhciHPack packageYaml)

runBuildResult :: BuildResult -> IO ()
runBuildResult = \case
    BuiltExecutable (ExecutableBuild p) -> runExe p
    BuiltMulti b
      | [ExecutableBuild exe] <- Map.elems (executableBuilds b) -> runExe exe
    b -> fail $ "Unexpected build type: " <> show b

quiet :: Sh a -> IO a
quiet = S.shelly . S.print_stdout False
runExe :: NixPath -> IO ()
runExe (NixPath fp) = executeFile (T.unpack fp) True [] Nothing

parseCommand :: Opts.Parser Command
parseCommand =
  Opts.hsubparser $
    ( Opts.command "build" (Opts.info (pure Build) mempty)
    <>  Opts.command "run" (Opts.info (pure Run) mempty)
    <>  Opts.command "ghci" (Opts.info (pure Ghci) mempty)
    )

run :: S.FilePath -> [T.Text] -> Sh [T.Text]
run p args = T.lines <$> S.run p args

runStdin1 :: T.Text -> S.FilePath -> [T.Text] -> Sh T.Text
runStdin1 stin p args = do
    S.setStdin stin
    run p args >>= \case
      [out] -> pure out
      xs -> throwIO $ userError $ "unexpected output: " <> show xs

run_ :: S.FilePath -> [T.Text] -> Sh ()
run_ p args = void $ run p args

specJson :: T.Text
specJson = $(embedStringFile "spec.json")

libb64 :: T.Text
libb64 = $(embedStringFile "lib.tar.gz.b64")
