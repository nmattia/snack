{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.List (stripPrefix)
#if __GLASGOW_HASKELL__ >= 804
#else
import Data.Semigroup
#endif
import System.Environment
import Control.Exception
import qualified DriverPipeline
import qualified DynFlags
import qualified FastString
import qualified GHC
import qualified ErrUtils
import qualified Bag
import qualified GHC.IO.Handle.Text as Handle
import qualified HsImpExp
import qualified HsSyn
import qualified HscTypes
import qualified Lexer
import qualified Module
import qualified Outputable
import qualified Parser
import qualified SrcLoc
import qualified StringBuffer
import qualified System.Process as Process
import System.IO (stderr)

main :: IO ()
main = do
    (fp:exts) <- getArgs >>= \case
      args@(_:_) -> pure args
      [] -> fail "Please provide at least one argument (got none)"

    -- Read the output of @--print-libdir@ for 'runGhc'
    (_,Just ho1, _, hdl) <- Process.createProcess
      (Process.shell "ghc --print-libdir"){Process.std_out=Process.CreatePipe}
    libdir <- filter (/= '\n') <$> Handle.hGetContents ho1
    _ <- Process.waitForProcess hdl

    -- Some gymnastics to make the parser happy
    res <- GHC.runGhc (Just libdir)
      $ do

        -- We allow passing some extra extensions to be parsed by GHC.
        -- Otherwise modules that have e.g. @RankNTypes@ enabled will fail to
        -- parse. Note: if anybody gets rid of this: even without this it /is/
        -- necessary to run getSessionFlags/setSessionFlags at least once,
        -- otherwise GHC parsing fails with the following error message:
        --    <command line>: unknown package: rts
        dflags0 <- GHC.getSessionDynFlags
        (dflags1, _leftovers, _warns) <-
          DynFlags.parseDynamicFlagsCmdLine dflags0 (map (SrcLoc.mkGeneralLocated "on the commandline") exts)
        _ <- GHC.setSessionDynFlags dflags1

        hsc_env <- GHC.getSession


        -- XXX: We need to preprocess the file so that all extensions are
        -- loaded
        (dflags2, fp2) <- liftIO $
#if __GLASGOW_HASKELL__ >= 808
          do
            eitherResult <- DriverPipeline.preprocess hsc_env fp Nothing Nothing
            case eitherResult of
              Left errors -> throwIO $ HscTypes.mkSrcErr errors
              Right result -> return result
#else
          DriverPipeline.preprocess hsc_env (fp, Nothing)
#endif
        _ <- GHC.setSessionDynFlags dflags2

        -- Read the file that we want to parse
        str <- liftIO $ filterBOM <$> readFile fp2

        runParser fp2 str Parser.parseModule >>= \case
          Lexer.POk _ (SrcLoc.L _ res) -> pure res
#if __GLASGOW_HASKELL__ >= 804
          Lexer.PFailed _ spn e -> liftIO $ do
#else
          Lexer.PFailed spn e -> liftIO $ do
#endif
            Handle.hPutStrLn stderr $ unlines
              [ "Could not parse module: "
              , fp2
              , " (originally " <> fp <> ")"
              , " because " <> Outputable.showSDocUnsafe e
              , " src span "
              , show spn
              ]
            throwIO $ HscTypes.mkSrcErr $
              Bag.unitBag $ ErrUtils.mkPlainErrMsg dflags2 spn e

    -- Extract the imports from the parsed module
    let imports' =
          map (\(SrcLoc.L _ idecl) ->
                  let SrcLoc.L _ n = HsImpExp.ideclName idecl
                  in Module.moduleNameString n) (HsSyn.hsmodImports res)

    -- here we pretend that @show :: [String] -> String@ outputs JSON
    print imports'

-- | Filter out the Byte Order Mark to avoid the following error:
-- lexical error at character '\65279'
filterBOM :: String -> String
filterBOM = \case
    [] -> []
    str@(x:xs) -> case stripPrefix "\65279" str of
      Just str' -> filterBOM str'
      Nothing -> x : filterBOM xs

runParser :: FilePath -> String -> Lexer.P a -> GHC.Ghc (Lexer.ParseResult a)
runParser filename str parser = do
    dynFlags <- DynFlags.getDynFlags
    pure $ Lexer.unP parser (parseState dynFlags)
  where
    location = SrcLoc.mkRealSrcLoc (FastString.mkFastString filename) 1 1
    buffer = StringBuffer.stringToStringBuffer str
    parseState flags = Lexer.mkPState flags buffer location
