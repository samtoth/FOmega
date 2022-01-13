{-# LANGUAGE ScopedTypeVariables #-}
module GhcDriver where

import Control.Exception
import CoreSyn (CoreProgram)
import qualified CoreSyn as Syn
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO as T
import Debug.Trace
import GHC (CoreModule, InteractiveImport (IIDecl), cm_binds, compileToCoreSimplified, guessTarget, mkModuleName, runGhc, setContext, setSessionDynFlags, setTargets, simpleImportDecl, gcatch)
import GHC.Paths (libdir)
import GhcMonad
import System.Directory
import System.Environment
import qualified System.IO as IO
import GhcPlugins

compileSource :: T.Text -> IO (Either String CoreModule)
compileSource t = do
  file <- writeSourceToTemp t
  ( do
      core <- compileToCore file
      cleanup
      pure . Right $ core
    )
    `gcatch` (\(e :: SourceError) -> failure e)
    `gcatch` (\(g :: GhcApiError) -> failure g)
    `gcatch` (\(se :: SomeException) -> failure se)
  where
    cleanup = do
      tempFile <- fmap (++ "/tempCoreDraw.hs") getTemporaryDirectory
      Debug.Trace.trace ("Cleaning " ++ tempFile) $ removeFile tempFile
    failure :: Show e => e -> IO (Either String a)
    failure = \e -> do
      let err = show e
      cleanup
      pure . Left $ err

writeSourceToTemp :: T.Text -> IO FilePath
writeSourceToTemp source = do
  tempFile <- fmap (++ "/tempCoreDraw.hs") getTemporaryDirectory
  Debug.Trace.trace ("Writing haskell to " ++ tempFile) $ T.writeFile tempFile source
  return tempFile

compileToCore :: FilePath -> IO CoreModule
compileToCore path = runGhc
  (Just libdir)
  $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    setContext [IIDecl $ simpleImportDecl (mkModuleName "Prelude")]
    compileToCoreSimplified path
