{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson ((.:))
import Data.ByteString as BS
import Data.FileEmbed (embedStringFile)
import Data.Semigroup
import Data.String (fromString)
import Data.String.Interpolate
import Data.Text as T
import Shelly (Sh)
import System.Posix.Process (executeFile)
import qualified Data.Aeson as Aeson
import qualified Options.Applicative as Opts
import qualified Shelly as S

{-

  TODO:
    Mode
      = HPack HPackPah
      | Standalone SnackNix
      | Discovery (Either HPackPath SnackNix)
-}

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

data Project = Project
  { outPath :: FilePath
  , exePath :: FilePath
  }

instance Aeson.FromJSON Project where
  parseJSON = Aeson.withObject "project" $ \o ->
    Project <$> o .: "out_path" <*> o .: "exe_path"

parseMode :: Opts.Parser Mode
parseMode =
    -- Opts.flag Standalone Standalone
      -- (Opts.long "standalone")

    ((Standalone . mkSnackNix) <$>
        Opts.strOption
        (Opts.long "snack-nix"
        <> Opts.short 's'
        <> Opts.value "./snack.nix"
        <> Opts.metavar "PATH")
        )
    <|>
    -- Opts.flag HPack HPack
      -- (Opts.long "hpack")

    ((HPack . mkPackageYaml) <$>
        Opts.strOption
        (Opts.long "package-yaml"
        <> Opts.value "./package.yaml"
        <> Opts.metavar "PATH")
        )

options :: Opts.Parser Options
options = Options <$> parseMode <*> parseCommand

snackGhci :: SnackNix -> Sh ()
snackGhci snackNix = do
    path <- S.print_stdout False $ exePath <$> snackBuildGhci snackNix
    S.print_stdout True $ run_ (fromString path) []

snackBuildGhci :: SnackNix -> Sh Project
snackBuildGhci snackNix = do
    out <- runStdin1
      (T.pack [i|
        { snackNix, lib64, specJson }:
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
          proj = snack.executable (import snackNix);
        in
          { build = proj.build;
            ghci = proj.ghci;
          }
      |]
      )
      "nix-build"
      [ "-"
      , "--arg", "snackNix", T.pack $ unSnackNix snackNix
      , "--argstr", "lib64", libb64
      , "--argstr", "specJson", specJson
      , "--no-out-link"
      , "-A", "ghci.json"
      ]
    json <- liftIO $ BS.readFile (T.unpack out)
    let Just proj = Aeson.decodeStrict' json
    pure proj

snackBuildHPack :: PackageYaml -> Sh Project
snackBuildHPack packageYaml = do
    out <- runStdin1
      (T.pack [i|
        { base, packageYaml, lib64, specJson }:
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
          proj = snack.packageYaml packageYaml;
        in
          { build = proj.library.build;
            ghci = proj.librayr.build;
          }
      |]
      )
      "nix-build"
      [ "-"
      , "--arg", "packageYaml", T.pack $ unPackageYaml packageYaml
      , "--argstr", "lib64", libb64
      , "--argstr", "specJson", specJson
      , "--arg", "base", "./app"
      , "--no-out-link"
      , "-A", "build.json"
      ]
    json <- liftIO $ BS.readFile (T.unpack out)
    let Just proj = Aeson.decodeStrict' json
    pure proj

snackBuild :: SnackNix -> Sh Project
snackBuild snackNix = do
    out <- runStdin1
      (T.pack [i|
        { snackNix, lib64, specJson }:
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
          proj = snack.executable (import snackNix);
        in
          { build = proj.build;
            ghci = proj.ghci;
          }
      |]
      )
      "nix-build"
      [ "-"
      , "--arg", "snackNix", T.pack $ unSnackNix snackNix
      , "--argstr", "lib64", libb64
      , "--argstr", "specJson", specJson
      , "--no-out-link"
      , "-A", "build.json"
      ]
    json <- liftIO $ BS.readFile (T.unpack out)
    let Just proj = Aeson.decodeStrict' json
    pure proj

runCommand :: Mode -> Command -> IO ()
runCommand (Standalone snackNix) = \case
  Build -> S.shelly $ void $ snackBuild snackNix
  Run -> snackRun snackBuild
  Ghci -> snackRun snackBuildGhci
  where
    snackRun build = do
      fp <- S.shelly $ S.print_stdout False $ exePath <$> build snackNix
      executeFile fp True [] Nothing
runCommand (HPack packageYaml) = \case
  Build -> S.shelly $ void $ snackBuildHPack packageYaml
  Run -> undefined -- snackRun snackBuild
  Ghci -> undefined -- snackRun snackBuildGhci
  -- where
    -- snackRun build = do
      -- fp <- S.shelly $ S.print_stdout False $ exePath <$> build packageYaml
      -- executeFile fp True [] Nothing

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
    [out] <- run p args
    pure out

run_ :: S.FilePath -> [T.Text] -> Sh ()
run_ p args = void $ run p args

specJson :: T.Text
specJson = $(embedStringFile "spec.json")

libb64 :: T.Text
libb64 = $(embedStringFile "lib.tar.gz.b64")
