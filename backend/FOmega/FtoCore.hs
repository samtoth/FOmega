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

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module FtoCore where

  import Data.FSyn as F
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
  import VarSet
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
  import SimplUtils (mkCase)
  import Data.Kind

  import qualified Data.HashMap.Strict as HM
  import Data.Foldable (foldl')


  data FTCReader = FTCReader {
    fr_mod :: !Module,
    fr_um :: Char,
    fr_localenv :: FTCLocal
  }

  data FTCLocal = FTCLocal {
    le_names  :: HM.HashMap FUName Name,
    le_vars   :: VarSet, -- Vars can be indexed by name
    le_tyVars :: TyVarSet
  }

  instance Semigroup FTCLocal where
    a <> b = FTCLocal {
      le_names = le_names a <> le_names b,
      le_vars = le_vars a <> le_vars b,
      le_tyVars = le_tyVars a <> le_tyVars b
    }

  instance Monoid FTCLocal where
    mempty = FTCLocal {
      le_names = mempty,
      le_vars = mempty,
      le_tyVars = mempty
    }

  data FTCWriter = FTCWriter {
      binds :: [Syn.CoreBind]
    , exports :: NameSet
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

  appExport :: Name -> FTCM ()
  appExport x = tell $ mempty {exports = unitNameSet x}

  appBind :: Syn.CoreBind -> FTCM ()
  appBind x = tell $ mempty {binds = [x]}

  getVarByName :: FUName -> FTCM Var
  getVarByName name = 
    do
      le <- asks fr_localenv
      let names = le_names le
          vars = le_vars le
          name' = HM.lookup name names
      case name' of
          Just name'' -> case lookupVarSetByName vars name'' of
            Just var -> return var
            Nothing -> fail "no var with given name" -- TODO: error handling  
          Nothing -> fail "Lookup fail"   -- TODO: error handling  


  newtype FTCM a = MkFTCM { unFTCm :: WriterT FTCWriter (IOEnv FTCReader) a }
      deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadWriter FTCWriter, MonadFail)

  instance MonadReader FTCReader FTCM where
    ask = MkFTCM $ lift getEnv
    local f fm = let ioenv = runWriterT.unFTCm $ fm
                  in  MkFTCM . WriterT $ updEnv f ioenv

  appEnv :: FTCLocal -> (FTCReader -> FTCReader)
  appEnv loc = \reader -> reader {fr_localenv = fr_localenv reader <> loc}

  withName :: (FUName, Name) -> FTCM a -> FTCM a
  withName (un, n) = local $ appEnv mempty {le_names = HM.singleton un n}

  withVar :: Var -> FTCM a -> FTCM a
  withVar v = local $ appEnv mempty {le_vars = unitVarSet v}

  registerNameInternal :: String -> FTCM Name
  registerNameInternal name = (mkInternalName <$> getUniqueM) <*> pure (mkOccName OccName.varName name) <*> pure noSrcSpan

  registerNameExternal :: String -> FTCM Name
  registerNameExternal name = mkExternalName <$> getUniqueM <*> asks fr_mod <*> pure (mkOccName OccName.varName name) <*> pure noSrcSpan


  instance MonadUnique FTCM where
    getUniqueSupplyM = asks fr_um >>= liftIO . mkSplitUniqSupply
    getUniqueM = asks fr_um >>= liftIO . uniqFromMask


  runFTCM :: FTCReader -> FTCM a -> IO (a, FTCWriter)
  runFTCM r = runIOEnv r . runWriterT . unFTCm

  execFTCM :: FTCReader -> FTCM a -> IO FTCWriter
  execFTCM r = runIOEnv r . execWriterT . unFTCm

  compileToCore :: FModule -> IO (DynFlags -> ModSummary, ModGuts)
  compileToCore mod@MkFM {fm_name, fm_bindings}
      = let mname = T.unpack fm_name
            modl = mkModule (stringToUnitId.T.unpack $ fm_name) (mkModuleName.T.unpack $ fm_name)
        in do
          FTCWriter {exports, tcs, insts, binds} <- execFTCM (FTCReader modl (error "Need to figure out what to use as char mask") mempty) $ compileMod mod
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
                  mg_exports         = Avail <$> nameSetElemsStable exports,
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
  compileMod MkFM {fm_bindings} = foldl' (flip compileDec) (pure ()) fm_bindings

  compileDec :: FDeclaration 0 -> FTCM a -> FTCM a
  compileDec dec rest = case dec of  -- N.b. this pattern match IS total because Data declerations can only happen
                      -- at Type level (1) not term level as guaranteed by constraint DataDec :: (1 <= n) ...
    F.ValDecl True (MkBinding (MkBndr name sig) fe) -> do
          eName <- registerNameExternal.T.unpack $ name
          withName (name, eName) $ do
            eTy <- typeToCore sig
            let var = mkGlobalVar VanillaId eName eTy vanillaIdInfo  --TODO: Investigate meaning of VanillaID
            withVar var $ do
              expr <- exprToCore fe
              appBind $ Syn.NonRec var expr
              appExport eName
              rest
    F.ValDecl False (MkBinding (MkBndr name sig) fe) -> do
          eName <- registerNameInternal.T.unpack $ name
          withName (name, eName) $ do
            eTy <- typeToCore sig
            let var = createLocalVar eName eTy
            withVar var $ do
              expr <- exprToCore fe
              appBind $ Syn.NonRec var expr
              rest
    F.ClassDecl fcd -> error "TODO!!!" >> rest
    F.InstanceDef fih -> error "TODO!!!" >> rest

  
  {- defunct because a bndr can either be referencing a var - eg. Var
      or declaring a Var . i.e. lambda abstraction
  bndrToCore :: FBndr 0 -> FTCM Syn.CoreBndr
  bndrToCore (MkBndr name sig) = do
              xn <- registerNameInternal.T.unpack $ name
              ty <- typeToCore sig
              pure $ mkLocalVar VanillaId xn ty vanillaIdInfo
  -}

  createLocalVar :: Name -> TyCoRep.Type -> Var
  createLocalVar name ty = mkLocalVar VanillaId name ty vanillaIdInfo --TODO: Investigate meaning of VanillaID

  exprToCore :: FExpr 0 -> FTCM Syn.CoreExpr
  exprToCore = \case
    Var (MkBndr name _) -> Syn.Var <$> getVarByName name
    App fe fe' -> Syn.App <$> exprToCore fe <*> exprToCore fe'
    Abs (MkBndr name sig) fe -> do
      ty <- typeToCore sig
      name' <- registerNameInternal.T.unpack $ name 
      let var = createLocalVar name' ty
      withName (name, name') $ withVar var $ do
        expr <- exprToCore fe
        return $ Syn.Lam var expr 
          
    Let fbs fe -> letRec fe fbs
      where letRec :: FExpr 0 -> [FBinding 0] -> FTCM Syn.CoreExpr
            letRec e ((MkBinding (MkBndr name sig) fe):fbs) = do
                eName <- registerNameInternal.T.unpack $ name
                withName (name, eName) $ do
                  eTy <- typeToCore sig
                  let var = createLocalVar eName eTy
                  withVar var $ do
                    expr <- exprToCore fe
                    let bndr = Syn.NonRec var expr
                    expr <- letRec e fbs
                    return $ Syn.Let bndr expr
            letRec e []       = exprToCore e

    Case fe fs fcas -> do
      let dty'' = typeof fe
      case dty'' of 
        Right dty' -> do
          expr <- exprToCore fe
          cty <- typeToCore fs
          dty <- typeToCore dty'
          dummyName <- registerNameInternal "dummy"
          let dummyVar = createLocalVar dummyName dty
          withName ("dummy", dummyName) $ withVar dummyVar $ do
            arms <- sequence (caseArmToCore <$> fcas)
            return $ Syn.Case expr dummyVar cty arms
        Left e -> fail e
    TypeApp fe fe' -> Syn.App <$> exprToCore fe <*> (Syn.Type <$> typeExprToCore fe')
    TypeAbs fb fe -> Syn.Lam <$> tyBndrToCore fb <*> exprToCore fe
    Lit fl -> error "Literals are TODO!"

  typeToCore :: (1 <= n) => FSig n -> FTCM TyCoRep.Type
  typeToCore = \case
    SVar fb -> TyCoRep.TyVarTy <$> tyBndrToCore fb
    SMap fs fs' -> TyCoRep.FunTy VisArg <$> typeToCore fs <*> typeToCore fs'   -- Vis arg indicates (->) invis indicates (=>) when constraints are implemented
    SForall fb fs -> TyCoRep.ForAllTy <$> (Bndr <$> tyBndrToCore fb <*> pure Specified) <*> typeToCore fs
    SApp fs fs' -> TyCoRep.AppTy <$> typeToCore fs <*> typeToCore fs'
    SLit fl -> error "Literals are TODO!"

  caseArmToCore :: FCaseArm 0 -> FTCM Syn.CoreAlt
  caseArmToCore = error "TODO!!"

  tyBndrToCore :: (1 <= n) => FBndr n -> FTCM Syn.CoreBndr
  tyBndrToCore (MkBndr name sig) = mkTyVar <$> (registerNameInternal . T.unpack) name <*> typeToCore sig


  typeExprToCore :: FExpr 1 -> FTCM TyCoRep.Type
  typeExprToCore = error "typeExprToCore"



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