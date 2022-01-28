{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments      #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module GhcDriver where

import Control.Exception
import CoreSyn (CoreProgram)
import qualified CoreSyn as Syn
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Debug.Trace
import GHC (CoreModule, InteractiveImport (IIDecl), cm_binds, compileToCoreSimplified, guessTarget, mkModuleName, runGhc, setContext, setSessionDynFlags, setTargets, simpleImportDecl, gcatch)
import GHC.Paths (libdir)
import GhcMonad
import System.Directory
import System.Environment
import qualified System.IO as IO

-- Core Passes
import CorePrep (corePrepPgm)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
import CmmInfo (cmmToRawCmm )
import CmmLint (cmmLint)
import CmmPipeline (cmmPipeline)
import CmmBuildInfoTables (emptySRT)
import AsmCodeGen ( nativeCodeGen )
import UniqSupply ( mkSplitUniqSupply, initUs_ )

import qualified Stream
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


{-
compileCore :: String -> ((DynFlags -> ModSummary), ModGuts) -> FilePath -> IO ()
compileCore name (summ, guts) path = runGhc
  (Just libdir)
  $ do
    dflags <- getSessionDynFlags

    setSessionDynFlags $ dflags { hscTarget = HscAsm, ghcLink = LinkBinary }

    dflags <- getSessionDynFlags
    env <- getSession

    setTargets [Target 
      { targetId = TargetModule (mkModuleName name)
      , targetAllowObjCode = True
      , targetContents = Nothing }]

    -- Prepares for code generation.
    prep <- liftIO $ corePrepPgm env (ms_location (summ dflags)) (mg_binds guts) (mg_tcs guts)

    -- Transform Core into STG
    stg <- liftIO $ coreToStg dflags (mg_module guts) prep

    -- STG Transformer
    (stg_binds2, cost_centre_info) <- liftIO $ stg2stg dflags (mg_module guts) stg

    -- Generated Cmm
    let cmms = codeGen dflags (mg_module guts) (mg_tcs guts) cost_centre_info stg_binds2 (mg_hpc_info guts)

    -- Initialize a name supply for the Cmm pipeline
    us <- liftIO $ mkSplitUniqSupply 'S'
    let initTopSRT = initUs_ us emptySRT
        run_pipeline = cmmPipeline env

    -- Collect the Cmm code stream after running the pipeline.
    let cmmstream = do {
      a <- Stream.mapAccumL run_pipeline initTopSRT cmms ;
      Stream.yield (srtToData a)
    }
    -- Prepare the Cmm for 
    genraw <- liftIO $ cmmToRawCmm dflags cmmstream

    -- Initialize name supply for the native code generator and generate x86 to a
    -- file from the prepared Cmm.
    ncg_uniqs <- liftIO $ mkSplitUniqSupply 'n'
    fname <- liftIO $ (IO.openFile (name <> ".asm") IO.WriteMode)
    liftIO $ nativeCodeGen dflags (mg_module guts) (mg_loc guts) fname ncg_uniqs genraw

    -- -- Dump the outputted Stg and  Cmm out
    -- gen <- liftIO $ Stream.collect cmmstream
    -- liftIO $ putStrLn "=== STG ==="
    -- liftIO $ putStrLn $ showGhc stg_binds2

    -- liftIO $ putStrLn "=== CMM ==="
    -- liftIO $ putStrLn $ showGhc gen

    -}