{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}

module FtoCore where

    import Data.FSyn
    import GHC.TypeLits hiding (Symbol)

    -- Compiler
    import GHC
    import DynFlags
    import HscMain
    import HscTypes

    -- Core Types
    import Var
    import Name
    import Avail
    import IdInfo
    import Module
    import Unique
    import OccName
    import InstEnv
    import NameSet
    import RdrName
    import FamInstEnv
    import TyCoRep
    import qualified Stream
    import qualified CoreSyn as Syn

    import Control.Monad.Reader
    import Control.Monad.Writer

    import UniqSupply


    import Control.Applicative ( Alternative(..) )

    import Data.Time.Clock
    import qualified Data.Text as T
    import qualified Data.Text.Encoding as T
    import IOEnv
    import Data.Functor
    import Data.Set
    import SimplUtils (mkCase)
    import Data.Kind
    

    data FTCReader = FTCReader !Module

    data FTCWriter = FTCWriter {
        binds :: [Syn.CoreBind]
      , exports :: Set Name
      -- , deps :: 
      , tcs :: [TyCon]
      , insts :: [ClsInst]
    }


    instance Semigroup FTCWriter where
      a <> b = FTCWriter {
          binds = binds a <> binds b,
          exports = exports a <> exports b,
          tcs = tcs a <> tcs b,
          insts = insts a <> insts b
      }

    instance Monoid FTCWriter where
      mempty = FTCWriter {
          binds = mempty,
          exports = mempty,
          tcs = mempty,
          insts = mempty
      }

    appName :: Name -> FTCM ()
    appName x = tell $ mempty {exports = singleton x}


    newtype FTCM a = MkFTCM { unFTCm :: WriterT FTCWriter (IOEnv FTCReader) a }
        deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadWriter FTCWriter)

    instance MonadReader FTCReader FTCM where
      ask = MkFTCM $ lift getEnv
      local f fm = let ioenv = runWriterT.unFTCm $ fm
                   in  MkFTCM . WriterT $ updEnv f ioenv

    instance MonadUnique FTCM where
      getUniqueSupplyM = _

    runFTCM :: FTCReader -> FTCM a -> IO (a, FTCWriter)
    runFTCM r = runIOEnv r . runWriterT . unFTCm

    execFTCM :: FTCReader -> FTCM a -> IO FTCWriter
    execFTCM r = runIOEnv r . execWriterT . unFTCm

    compileToCore :: FModule -> IO (DynFlags -> ModSummary, ModGuts)
    compileToCore mod@MkFM {fm_name, fm_bindings}
        = let mname = T.unpack fm_name
              modl = mkModule (stringToUnitId.T.unpack $ fm_name) (mkModuleName.T.unpack $ fm_name)
          in do
            FTCWriter {exports, tcs, insts, binds} <- execFTCM (FTCReader modl) $ compileMod mod
            let deps = noDependencies
            pure (
                \dflags -> ModSummary
                    {
                        ms_mod          = modl,
                        ms_hsc_src      = HsSrcFile,
                        ms_location     = ModLocation {
                            ml_hs_file  = Nothing
                        ,   ml_hie_file = mname <> ".hie"
                        ,   ml_hi_file  = mname <> ".hi"
                        ,   ml_obj_file = mname <> ".o"
                        },
                        ms_hs_date      = UTCTime (toEnum 0) 0,
                        ms_obj_date     = Nothing,
                        ms_iface_date   = Nothing,
                        ms_srcimps      = [],
                        ms_textual_imps = [],
                        ms_hspp_file    = mname <> ".hs",
                        ms_hspp_opts    = dflags,
                        ms_hspp_buf     = Nothing,
                        ms_hie_date     = Nothing,
                        ms_parsed_mod   = Nothing
                    },
                ModGuts
                {
                    mg_module          = modl,
                    mg_hsc_src         = HsSrcFile,
                    mg_loc             = noSrcSpan,
                    mg_exports         = fmap Avail . toList$exports,
                    mg_deps            = deps,
                    mg_usages          = [],  -- TODO!
                    mg_used_th         = False,
                    mg_rdr_env         = emptyGlobalRdrEnv,
                    mg_fix_env         = emptyFixityEnv,
                    mg_tcs             = tcs,
                    mg_insts           = insts,
                    mg_fam_insts       = [],
                    mg_patsyns         = [],
                    mg_rules           = [],
                    mg_binds           = binds,                  -- our bindings
                    mg_foreign         = NoStubs,
                    mg_foreign_files   = [],
                    mg_warns           = NoWarnings,
                    mg_hpc_info        = NoHpcInfo False,
                    mg_complete_sigs   = [],
                    mg_modBreaks       = Nothing,
                    mg_anns            = [],
                    mg_inst_env        = emptyInstEnv,
                    mg_fam_inst_env    = emptyFamInstEnv,
                    mg_safe_haskell    = Sf_None,
                    mg_trust_pkg       = False,
                    mg_doc_hdr         = Nothing,
                    mg_decl_docs       = emptyDeclDocMap,
                    mg_arg_docs        = emptyArgDocMap
                }
                )


    compileMod :: FModule -> FTCM ()
    compileMod = _

    bindToCore :: FBinding 0 -> FTCM Syn.CoreBind 
    bindToCore = _

    exprToCore :: FExpr 0 -> FTCM Syn.CoreExpr
    exprToCore = \case
      Var bndr -> Syn.Var <$> bndrToCore bndr
      App fe fe' -> Syn.App <$> exprToCore fe <*> exprToCore fe'
      Abs fb fe -> Syn.Lam <$> bndrToCore fb <*> exprToCore fe
      Let fbs fe -> letRec fe fbs
                    where letRec :: FExpr 0 -> [FBinding 0] -> FTCM Syn.CoreExpr
                          letRec e (fb:fbs) = Syn.Let <$> bindToCore fb <*> letRec e fbs
                          letRec e []       = exprToCore e
      Case fe fs fcas -> let Right t = typeof fe in Syn.Case <$> exprToCore fe <*> bndrToCore (MkBndr "dummy" t) <*> typeToCore fs <*> sequence (caseArmToCore <$> fcas)
      TypeApp fe fe' -> Syn.App <$> exprToCore fe <*> (Syn.Type <$> typeExprToCore fe')
      TypeAbs fb fe -> Syn.Lam <$> tyBndrToCore fb <*> exprToCore fe
      Lit fl -> error "Literals are TODO!"
            
    
    caseArmToCore :: FCaseArm 0 -> FTCM Syn.CoreAlt
    caseArmToCore = _

    type NplusM :: Constraint
    type NplusM = forall n. (1 <= n) => (1 <= n + 1)

    tyBndrToCore :: (1 <= n, Data.FSyn.Sig n ~ FSig (n + 1)) => FBndr n -> FTCM Syn.CoreBndr
    tyBndrToCore (MkBndr name sig) = mkTyVar <$> (mkName . T.unpack) name <*> typeToCore sig

    bndrToCore :: FBndr 0 -> FTCM Syn.CoreBndr
    bndrToCore (MkBndr name sig) = do
                xn <- mkName.T.unpack $ name
                ty <- typeToCore sig
                pure $ mkLocalVar VanillaId xn ty vanillaIdInfo

    mkName :: String -> FTCM Name
    mkName name = (mkInternalName <$> getUniqueM) <*> pure (mkOccName OccName.varName name) <*> pure noSrcSpan

    typeExprToCore :: FExpr 1 -> FTCM TyCoRep.Type
    typeExprToCore = _

    typeToCore :: (1 <= n) => FSig n -> FTCM TyCoRep.Type
    typeToCore = \case
      SVar fb -> TyCoRep.TyVarTy <$> tyBndrToCore fb
      SMap fs fs' -> TyCoRep.FunTy _ <$> typeToCore fs <*> typeToCore fs'
      SForall fb fs -> TyCoRep.ForAllTy <$> (Bndr <$> tyBndrToCore fb <*> pure Specified) <*> typeToCore fs
      SApp fs fs' -> TyCoRep.AppTy <$> typeToCore fs <*> typeToCore fs'
      SLit fl -> error "Literals are TODO!"


{-
ModGuts	 

    mg_module :: !Module             Module being compiled
    mg_hsc_src :: HscSource          Whether it's an hs-boot module
    mg_loc :: SrcSpan                For error messages from inner passes
    mg_exports :: ![AvailInfo]       What it exports
    mg_deps :: !Dependencies         What it depends on, directly or otherwise
    mg_usages :: ![Usage]            What was used? Used for interfaces.
    mg_used_th :: !Bool              Did we run a TH splice?
    mg_rdr_env :: !GlobalRdrEnv      Top-level lexical environment
    mg_fix_env :: !FixityEnv         Fixities declared in this module. Used for creating interface files.
    mg_tcs :: ![TyCon]               TyCons declared in this module (includes TyCons for classes)
    mg_insts :: ![ClsInst]           Class instances declared in this module
    mg_fam_insts :: ![FamInst]       Family instances declared in this module
    mg_patsyns :: ![PatSyn]          Pattern synonyms declared in this module
    mg_rules :: ![CoreRule]          Before the core pipeline starts, contains See Note [Overall plumbing for rules] in Rules.hs
    mg_binds :: !CoreProgram         Bindings for this module
    mg_foreign :: !ForeignStubs      Foreign exports declared in this module
    mg_foreign_files :: ![(ForeignSrcLang, FilePath)]
                                     Files to be compiled with the C compiler
    mg_warns :: !Warnings            Warnings declared in the module
    mg_anns :: [Annotation]          Annotations declared in this module
    mg_complete_sigs :: [CompleteMatch]
                                     Complete Matches
    mg_hpc_info :: !HpcInfo          Coverage tick boxes in the module
    mg_modBreaks :: !(Maybe ModBreaks)
                                     Breakpoints for the module
    mg_inst_env :: InstEnv           Class instance environment for home-package modules (including this one); c.f. tcg_inst_env
    mg_fam_inst_env :: FamInstEnv    Type-family instance environment for home-package modules (including this one); c.f. tcg_fam_inst_env
    mg_safe_haskell :: SafeHaskellMode    Safe Haskell mode
    mg_trust_pkg :: Bool             Do we need to trust our own package for Safe Haskell? See Note [RnNames . Trust Own Package]
    mg_doc_hdr :: !(Maybe HsDocString)    Module header.
    mg_decl_docs :: !DeclDocMap      Docs on declarations.
    mg_arg_docs :: !ArgDocMap
-}

{-
    mkName :: Int -> String -> Name
    mkName i n = mkInternalName (mkUnique 'n' i) (mkOccName OccName.varName n) noSrcSpan

    xn :: Name
    xn = mkName 0 "x"

    an :: Name
    an = mkName 1 "a"

    fn :: Name
    fn = mkExternalName (mkUnique 'n' 2) modl (mkOccName OccName.varName "f") noSrcSpan

    -- a :: *
    a :: TyVar
    a = mkTyVar an anyKind

    -- x :: a
    x :: Var
    x = mkLocalVar VanillaId xn (TyVarTy a) vanillaIdInfo

    -- f :: a -> a
    fv :: Var
    fv = mkGlobalVar VanillaId fn (FunTy (TyVarTy a) (TyVarTy a)) vanillaIdInfo

    def :: [Syn.CoreBind]
    def = [Syn.NonRec fv f]

    f :: Syn.Expr Var
    f = Syn.Lam x (Syn.Var x)
-}