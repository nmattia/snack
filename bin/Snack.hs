{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, (.:))
import Data.FileEmbed (embedStringFile)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Data.String.Interpolate
import Shelly (Sh)
import System.Directory (doesFileExist, doesPathExist, canonicalizePath)
import System.Posix.Process (executeFile)
import UnliftIO.Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Options.Applicative as Opts
import qualified Shelly as S

main :: IO ()
main = do
    opts <-
      prepareOptions =<<
        Opts.execParser (Opts.info (parseOptions <**> Opts.helper) mempty)
    runCommand (snackConfig opts) (package opts) (command opts)

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

--- Some config helpers

data ConfigStage
  = ConfigRaw
  | ConfigReady

type family Config (c :: ConfigStage) ty1 ty2 where
  Config 'ConfigRaw ty1 _ = ty1
  Config 'ConfigReady _ ty2 = ty2

--- Configuration proper

-- | Like a FilePath, but Nix friendly
newtype SnackNix = SnackNix FilePath

data SnackNixConfig
  = SnackNixSpecific FilePath
  | SnackNixDiscovery
  | SnackNixNone

parseSnackNixConfig :: Opts.Parser SnackNixConfig
parseSnackNixConfig =
    SnackNixSpecific <$> Opts.strOption
        (Opts.long "snack-nix"
        <> Opts.short 's'
        <> Opts.metavar "PATH"
        <> Opts.help (unlines
          [ "Use the specified environment (snack.nix) file."
          , "When none is provided ./snack.nix is used (if it exists)."
          , "(Use --no-snack-nix to disable this behavior)"
          ])
        ) <|>
    Opts.flag'
        SnackNixNone
        (Opts.long "no-snack-nix"
        <> Opts.help "Don't use ./snack.nix as the environment (snack.nix) file."
        ) <|>
    pure SnackNixDiscovery

prepareSnackNix :: SnackNixConfig -> IO (Maybe SnackNix)
prepareSnackNix = \case
    SnackNixNone -> pure Nothing
    SnackNixSpecific fp -> Just <$> mkSnackNix fp
    SnackNixDiscovery -> discoverSnackNix

discoverSnackNix :: IO (Maybe SnackNix)
discoverSnackNix = do
    eSNix <- mkSnackNixEither "snack.nix"
    case eSNix of
      Left{} -> pure Nothing
      Right sn -> pure (Just sn)

mkSnackNix :: FilePath -> IO SnackNix
mkSnackNix = fmap SnackNix . mkFilePath

mkSnackNixEither :: FilePath -> IO (Either String SnackNix)
mkSnackNixEither fp = fmap SnackNix <$> mkFilePathEither fp

-- | Like a FilePath, but Nix friendly
newtype SnackLib = SnackLib FilePath

mkSnackLib :: FilePath -> IO SnackLib
mkSnackLib = fmap SnackLib . mkDirPath

--- Package description (@package.yaml@, @package.nix@)

-- | Like a FilePath, but Nix friendly
newtype CanonicalFilePath = CanonicalFilePath { unpackCanonicalFilePath :: FilePath }

-- | What package description (@package.yaml@, @package.nix@) to use
data PackageFileConfig
  = PackageFileSpecific FilePath
    -- ^ Use the specified file as package description
  | PackageFileDiscovery
    -- ^ Find a suitable package description

parsePackageFileConfig :: Opts.Parser PackageFileConfig
parsePackageFileConfig =
    (PackageFileSpecific <$>
        Opts.strOption
        (Opts.long "package-file"
        <> Opts.short 'p'
        <> Opts.help (unlines
          [ "Specifies a YAML or Nix file to use as package description."
          , "If not provided, snack looks for either 'package.yaml' or 'package.nix' in the current directory."
          ]
          )
        <> Opts.metavar "PATH")
        ) <|> pure PackageFileDiscovery

-- Finding the package descriptions

mkCanonicalFilePathEither :: FilePath -> IO (Either String CanonicalFilePath)
mkCanonicalFilePathEither = fmap (fmap CanonicalFilePath) . mkFilePathEither

mkCanonicalFilePath :: FilePath -> IO CanonicalFilePath
mkCanonicalFilePath = fmap CanonicalFilePath . mkFilePath

preparePackage :: PackageFileConfig -> IO CanonicalFilePath
preparePackage = \case
    PackageFileSpecific fp -> mkCanonicalFilePath fp
    PackageFileDiscovery -> discoverPackageFile

-- | Tries to find a package description.
discoverPackageFile :: IO CanonicalFilePath
discoverPackageFile = do
    eYaml <- mkCanonicalFilePathEither "package.yaml"
    eNix <- mkCanonicalFilePathEither "package.nix"
    case (eYaml, eNix) of
      (Right (CanonicalFilePath yaml), Right (CanonicalFilePath nix)) ->
        throwIO $ userError $ unlines
          [ "Please specify which package file to use, e.g.: "
          , "  snack -p " <> yaml, "or"
          , "  snack -p " <> nix
          ]
      (Right yaml, Left{}) -> pure yaml
      (Left{}, Right nix) -> pure nix
      (Left e1, Left e2) -> throwIO $ userError $ unlines
        [ "Could not discover package file:"
        , e1, e2
        , "Please specify one with e.g.:"
        , "  snack -p <path-to-yaml-or-nix>"
        ]

--- Nix configuration

-- | How to call @nix-build@
newtype NixConfig = NixConfig
  { nixNJobs :: NJobs }

data NJobs = NJobs Int | NJobsAuto

nJobsValue :: NJobs -> String
nJobsValue = \case
  NJobs n -> show n
  NJobsAuto -> "auto"

parseNixConfig :: Opts.Parser NixConfig
parseNixConfig =
    NixConfig <$>
    (
      (NJobs <$> Opts.option Opts.auto
        (Opts.long "jobs"
        <> Opts.short 'j'
        <> Opts.metavar "INT"
        <> Opts.help "How many jobs to run concurrently (default: number of available cores)")
        ) <|>
      pure NJobsAuto
    )


--- Snack configuration (unrelated to packages)

type SnackConfig = SnackConfig_ 'ConfigReady
type SnackConfigRaw = SnackConfig_ 'ConfigRaw

-- | Extra configuration for snack
data SnackConfig_ c = SnackConfig
  { snackLib :: Maybe (Config c FilePath SnackLib)
  , snackNix :: Config c SnackNixConfig (Maybe SnackNix)
  , snackNixCfg :: NixConfig
  }

prepareSnackConfig :: SnackConfigRaw -> IO SnackConfig
prepareSnackConfig raw =
    SnackConfig <$>
      forM (snackLib raw) mkSnackLib <*>
      prepareSnackNix (snackNix raw) <*>
      pure (snackNixCfg raw)

parseSnackConfig :: Opts.Parser SnackConfigRaw
parseSnackConfig = SnackConfig <$> Opts.optional
    (Opts.strOption
        (Opts.long "lib"
        <> Opts.short 'l'
        <> Opts.metavar "DIR"
        <> Opts.help
          (unwords
            [ "Path to the directory to use as the Nix library"
            , "instead of the default one bundled with the snack executable."
            ]
          )
        )
    ) <*> parseSnackNixConfig <*>
    parseNixConfig

-- | What command to execute
data Command
  = Build
  | Run [String] -- Run with extra args
  | Ghci [String]
  | Test
  | Hoogle
  | HieBios String

parseCommand :: Opts.Parser Command
parseCommand =
  Opts.hsubparser
    ( Opts.command "build" (Opts.info (pure Build) mempty)
    <>  Opts.command "run" (Opts.info
        ( Run <$> Opts.many (Opts.argument Opts.str (Opts.metavar "ARG"))
        ) mempty)
    <>  Opts.command "ghci" (Opts.info
        ( Ghci <$> Opts.many (Opts.argument Opts.str (Opts.metavar "ARG"))
        ) mempty)
    <>  Opts.command "hoogle" (Opts.info (pure Hoogle) mempty)
    <>  Opts.command "hie-bios" (Opts.info
        ( HieBios <$> Opts.argument Opts.str (Opts.metavar "FILE")
        ) mempty)
    )
  <|> Opts.hsubparser
    ( Opts.command "test" (Opts.info (pure Test) (Opts.progDesc "Use build, run or ghci commands with test suites."))
    <> Opts.commandGroup "Unavailable commands:"
    )

type OptionsRaw = Options_ 'ConfigRaw
type Options = Options_ 'ConfigReady

-- | The whole set of CLI options
data Options_ c = Options
  { snackConfig :: SnackConfig_ c
  , package :: Config c PackageFileConfig CanonicalFilePath
  , command :: Command
  }

prepareOptions :: OptionsRaw -> IO Options
prepareOptions raw =
    Options <$>
      prepareSnackConfig (snackConfig raw) <*>
      preparePackage (package raw) <*>
      pure (command raw)

parseOptions :: Opts.Parser OptionsRaw
parseOptions =
    Options <$>
      parseSnackConfig <*>
      parsePackageFileConfig <*>
      parseCommand

--- Build related types used when interfacing with Nix

newtype ModuleName = ModuleName T.Text
  deriving newtype (Ord, Eq, Aeson.FromJSONKey)
  deriving stock Show

data BuildResult
  = BuiltLibrary LibraryBuild
  | BuiltExecutable ExecutableBuild
  | BuiltGhci GhciBuild
  | BuiltHoogle HoogleBuild
  | BuiltHieBios HieBiosBuild
  deriving Show

instance Aeson.FromJSON BuildResult where
    parseJSON v =
      BuiltLibrary <$> (guardBuildType "library" v)
      <|> BuiltExecutable <$> (guardBuildType "executable" v)
      <|> BuiltGhci <$> (guardBuildType "ghci" v)
      <|> BuiltHoogle <$> (guardBuildType "hoogle" v)
      <|> BuiltHieBios <$> (guardBuildType "hie-bios" v)
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
    deriving newtype FromJSON

-- The kinds of builds: library, executable, or a mix of both (currently only
-- for HPack)
newtype LibraryBuild = LibraryBuild (Map.Map ModuleName NixPath)
  deriving newtype FromJSON
  deriving stock Show

newtype ExecutableBuild = ExecutableBuild NixPath
  deriving stock Show

instance FromJSON ExecutableBuild where
  parseJSON = Aeson.withObject "executable build" $ \o ->
    ExecutableBuild <$> o .: "exe_path"

newtype HoogleBuild = HoogleBuild
  { hoogleExePath :: NixPath
  }
  deriving stock Show

instance FromJSON HoogleBuild where
  parseJSON = Aeson.withObject "hoogle build" $ \o ->
    HoogleBuild <$> o .: "exe_path"

newtype HieBiosBuild = HieBiosBuild
  { hieBiosFlags :: [T.Text]
  }
  deriving stock Show

instance FromJSON HieBiosBuild where
  parseJSON = Aeson.withObject "hie-bios build" $ \o ->
    HieBiosBuild <$> o .: "hie-bios_flags"

data NixArg = NixArg
  { argType :: NixArgType
  , argName :: T.Text
  , argValue :: T.Text
  }

data NixArgType
  = ArgStr
  | Arg

newtype NixExpr = NixExpr { unNixExpr :: T.Text }

newtype NixPath = NixPath T.Text
  deriving newtype FromJSON
  deriving stock Show

nixBuild :: SnackConfig -> [NixArg] -> NixExpr -> Sh NixPath
nixBuild snackCfg extraNixArgs nixExpr =
    NixPath <$> runStdin1
      (T.pack [i|
        { #{ intercalate "," funArgs } }:
        let
          spec = builtins.fromJSON specJson;
          config = #{ pkgsSrc };
          pkgs = config.pkgs;
          libDir = #{ libDir };
          snack = (import libDir) config;
        in #{ T.unpack $ unNixExpr $ nixExpr }
      |])
      "nix-build"
      cliArgs
  where
    pkgsSrc :: String
    pkgsSrc =  case snackNix snackCfg of
      Just (SnackNix fp) ->
        [i|(import #{ fp })|]
      Nothing ->
        [i|
        { pkgs = import
            (
            builtins.fetchTarball { inherit (spec) url sha256; }
            ) {} ;
        }
        |]
    libDir :: String
    libDir = case snackLib snackCfg of
      Just (SnackLib fp) -> fp
      Nothing ->
        [i|
              let
                b64 = pkgs.writeTextFile { name = "lib-b64"; text = lib64; };
              in
                pkgs.runCommand "snack-lib" {}
                ''
                  cat ${b64} | base64 --decode > out.tar.gz
                  mkdir -p $out
                  tar -C $out -xzf out.tar.gz
                  chmod +w $out
                ''
        |]
    cliArgs :: [T.Text]
    cliArgs =
      [ "-" -- read expression from stdin
      , "--no-out-link" -- no need for roots
      -- how many jobs to run concurrently (-j)
      , "--max-jobs", T.pack (nJobsValue (nixNJobs nixCfg))
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
    nixCfg = snackNixCfg snackCfg

snackBuild :: SnackConfig -> CanonicalFilePath -> Sh [BuildResult]
snackBuild snackCfg packageFile = do
    NixPath out <- nixBuild snackCfg
      [ NixArg
          { argName = "packageFile"
          , argValue = T.pack $ unpackCanonicalFilePath packageFile
          , argType = Arg
          }
      ]
      $ NixExpr "snack.inferBuild packageFile"
    decodeOrFail =<< liftIO (BS.readFile $ T.unpack out)

snackGhci :: SnackConfig -> CanonicalFilePath -> Sh GhciBuild
snackGhci snackCfg packageFile = do
    NixPath out <- nixBuild snackCfg
      [ NixArg
          { argName = "packageFile"
          , argValue = T.pack $ unpackCanonicalFilePath packageFile
          , argType = Arg
          }
      ]
      $ NixExpr "snack.inferGhci packageFile"
    liftIO (BS.readFile (T.unpack out)) >>= decodeOrFail >>= \case
      -- TODO: shouldn't be dropping the tail
      (BuiltGhci g):_ -> pure g
      bs -> throwIO $ userError $ "Expected GHCi build, got " <> show bs


snackHoogle :: SnackConfig -> CanonicalFilePath -> Sh HoogleBuild
snackHoogle snackCfg packageFile = do
    NixPath out <- nixBuild snackCfg
      [ NixArg
          { argName = "packageFile"
          , argValue = T.pack $ unpackCanonicalFilePath packageFile
          , argType = Arg
          }
      ]
      $ NixExpr "snack.buildHoogle packageFile"
    liftIO (BS.readFile (T.unpack out)) >>= decodeOrFail >>= \case
      BuiltHoogle hb -> pure hb
      bs -> throwIO $ userError $ "Expected Hoogle build, got " <> show bs

snackHieBios :: SnackConfig -> CanonicalFilePath -> CanonicalFilePath -> Sh HieBiosBuild
snackHieBios snackCfg packageFile haskellFile = do
    NixPath out <- nixBuild snackCfg
      [ NixArg
          { argName = "packageFile"
          , argValue = T.pack $ unpackCanonicalFilePath packageFile
          , argType = Arg
          }
      , NixArg
          { argName = "haskellFile"
          , argValue = T.pack $ unpackCanonicalFilePath haskellFile
          , argType = Arg
          }
      ]
      $ NixExpr "snack.buildHieBios packageFile haskellFile"
    liftIO (BS.readFile (T.unpack out)) >>= decodeOrFail >>= \case
      BuiltHieBios hb -> pure hb
      bs -> throwIO $ userError $ "Expected hie-bios flags, got " <> show bs

runCommand :: SnackConfig -> CanonicalFilePath -> Command -> IO ()
runCommand snackCfg packageFile = \case
  Build -> S.shelly $ void $ snackBuild snackCfg packageFile
  Run args -> quiet (snackBuild snackCfg packageFile) >>= runBuildResult args
  Ghci args -> flip runExe args =<<
    ghciExePath <$> (quiet (snackGhci snackCfg packageFile))
  Test -> noTest
  Hoogle -> flip runExe [ "server", "--local" ] =<<
    hoogleExePath <$> S.shelly (snackHoogle snackCfg packageFile)
  HieBios haskellFile -> S.shelly $ do
    maybeBiosFile <- S.get_env "HIE_BIOS_OUTPUT"
    biosFile <- case maybeBiosFile of
      Nothing -> fail $ "HIE_BIOS_OUTPUT environment variable not set"
      Just x -> pure x
    canonicalHaskellFile <- liftIO $ mkCanonicalFilePath haskellFile
    result <- snackHieBios snackCfg packageFile canonicalHaskellFile
    let flags = hieBiosFlags result
    S.writefile (S.fromText biosFile) (T.unlines flags)


noTest :: IO a
noTest = fail "There is no test command for test suites"

runBuildResult :: [String] -> [BuildResult] -> IO ()
runBuildResult args res =
    case mapMaybe (\case {BuiltExecutable e -> Just e; _ -> Nothing}) res of
      [ExecutableBuild p] -> runExe p args
      -- TODO: be more graceful here
      -- TODO: allow 'snack run my-exe -- ...'
      _ -> fail $ "Expected exactly one executable, got: " <> show res

runExe :: NixPath -> [String] -> IO ()
runExe (NixPath fp) args = executeFile (T.unpack fp) True args Nothing

specJson :: T.Text
specJson = $(embedStringFile "spec.json")

libb64 :: T.Text
libb64 = $(embedStringFile "lib.tar.gz.b64")

--- Auxiliary

mkDirPath :: FilePath -> IO FilePath
mkDirPath fp = doesPathExist fp >>= \case
    True -> doesFileExist fp >>= \case
      True -> throwIO $ userError $ fp <> " is a file"
      False -> canonicalizePath fp
    False -> throwIO $ userError $ fp <> " does not exist"

mkFilePath :: FilePath -> IO FilePath
mkFilePath fp =
    mkFilePathEither fp >>= either (throwIO . userError) pure

mkFilePathEither :: FilePath -> IO (Either String FilePath)
mkFilePathEither fp = doesFileExist fp >>= \case
    True -> Right <$> canonicalizePath fp
    False -> doesPathExist fp >>= \case
      True -> pure (Left (fp <> " is a directory"))
      False -> pure (Left (fp <> " does not exist"))

decodeOrFail :: FromJSON a => BS.ByteString -> Sh a
decodeOrFail bs = case Aeson.decodeStrict' bs of
    Just foo -> pure foo
    Nothing -> throwIO $ userError $ unlines
      [ "could not decode " <> show bs ]

-- | Run the executable with given arguments
run :: S.FilePath -> [T.Text] -> Sh [T.Text]
run p args = T.lines <$> S.run p args

-- | Run the executable with given arguments, assuming a single line of output
runStdin1 :: T.Text -> S.FilePath -> [T.Text] -> Sh T.Text
runStdin1 stin p args = do
    S.setStdin stin
    run p args >>= \case
      [out] -> pure out
      xs -> throwIO $ userError $ "unexpected output: " <> show xs

quiet :: Sh a -> IO a
quiet = S.shelly . S.print_stdout False
