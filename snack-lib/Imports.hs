{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

import Control.Monad.IO.Class
import Data.Semigroup
import System.Environment
import qualified DriverPipeline
import qualified DynFlags
import qualified FastString
import qualified GHC
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

main :: IO ()
main = do
    fp <- getArgs >>= \case
      [fp] -> pure fp
      [] -> fail "Please provide exactly one argument (got none)"
      xs -> fail $ "Please provide exactly one argument, got: \n" <> unlines xs

    -- Read the output of @--print-libdir@ for 'runGhc'
    (_,Just ho1, _, hdl) <- Process.createProcess
      (Process.shell "ghc --print-libdir"){Process.std_out=Process.CreatePipe}
    libdir <- filter (/= '\n') <$> Handle.hGetContents ho1
    _ <- Process.waitForProcess hdl

    -- Read the file that we want to parse
    str <- readFile fp

    -- Some gymnastics to make the parser happy
    res <- GHC.runGhc (Just libdir)
      $ do

        hsc_env <- GHC.getSession
        -- XXX: We need to preprocess the file so that all extensions are
        -- loaded
        (dflags, _) <- liftIO $ DriverPipeline.preprocess hsc_env (fp, Nothing)
        hsc_env <- GHC.setSession hsc_env { HscTypes.hsc_dflags = dflags }

        runParser fp str (Parser.parseModule) >>= \case
          Lexer.POk _ (SrcLoc.L _ res) -> pure res
          Lexer.PFailed _ e -> fail $ unlines
            [ "Could not parse module: "
            , fp
            , " because " <> Outputable.showSDocUnsafe e
            ]

    -- Extract the imports from the parsed module
    let imports' =
          map (\(SrcLoc.L _ idecl) ->
                  let SrcLoc.L _ n = HsImpExp.ideclName idecl
                  in Module.moduleNameString n) (HsSyn.hsmodImports res)

    -- here we pretend that @show :: [String] -> String@ outputs JSON
    print imports'

runParser :: FilePath -> String -> Lexer.P a -> GHC.Ghc (Lexer.ParseResult a)
runParser filename str parser = do
    dynFlags <- DynFlags.getDynFlags
    pure $ Lexer.unP parser (parseState dynFlags)
  where
    location = SrcLoc.mkRealSrcLoc (FastString.mkFastString filename) 1 1
    buffer = StringBuffer.stringToStringBuffer str
    parseState flags = Lexer.mkPState flags buffer location
