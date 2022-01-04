module GhcDriver where

import Control.Exception
import CoreSyn (CoreProgram)
import qualified CoreSyn as Syn
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO as T
import Debug.Trace
import GHC (CoreModule, InteractiveImport (IIDecl), cm_binds, compileToCoreSimplified, guessTarget, mkModuleName, runGhc, setContext, setSessionDynFlags, setTargets, simpleImportDecl)
import GHC.Paths (libdir)
import GhcMonad
import System.Directory
import System.Environment
import qualified System.IO as IO

compileSource :: T.Text -> IO (Either String CoreModule)
compileSource t = do
  file <- writeSourceToTemp t
  catch
    ( do
        core <- compileToCore file
        cleanup
        pure . Right $ core
    )
    ( \e -> do
        let err = show (e :: IOException)
        cleanup
        pure . Left $ ("Error: fail in compilation: " ++ err)
    )
  where
    cleanup = do
      tempFile <- fmap (++ "/tempCoreDraw.hs") getTemporaryDirectory
      Debug.Trace.trace ("Cleaning " ++ tempFile) $ removeFile tempFile

writeSourceToTemp :: T.Text -> IO FilePath
writeSourceToTemp source = do
  tempFile <- fmap (++ "/tempCoreDraw.hs") getTemporaryDirectory
  Debug.Trace.trace ("Writing haskell to " ++ tempFile) $ T.writeFile tempFile source
  return tempFile

compileToCore :: FilePath -> IO CoreModule
compileToCore path =
  Debug.Trace.trace
    "About to compile to Core"
    ( runGhc
        (Just libdir)
        $ do
          dflags <- getSessionDynFlags
          setSessionDynFlags dflags
          setContext [IIDecl $ simpleImportDecl (mkModuleName "Prelude")]
          compileToCoreSimplified path
    )
