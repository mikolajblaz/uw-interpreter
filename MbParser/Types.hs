module Types where

import Control.Monad.Reader
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment

-- | Data environment
type DataEnv = Map.Map TyCon ([TyVar], Map.Map Con [Type])

-- | Build data environment basing on 'data' declarations
buildDataEnv :: [TopDecl] -> Err DataEnv
buildDataEnv [] = Ok Map.empty
buildDataEnv x = Bad $ show x

----------------- Static type check ---------------------------
type StaticType = StaticVal Type
type TypeM = EvalM Type

-- TODO: remove all but Bool and Int maybe
boolType, intType, doubleType, charType, stringType :: Type
boolType = GTyCon $ SimpleTyCon $ ConTyCon $ Con "Bool"
intType = GTyCon $ SimpleTyCon $ ConTyCon $ Con "Int"
doubleType = GTyCon $ SimpleTyCon $ ConTyCon $ Con "Double"
charType = GTyCon $ SimpleTyCon $ ConTyCon $ Con "Char"
stringType = GTyCon $ SimpleTyCon $ ConTyCon $ Con "String"

instance EnvVal Type where
  getVal (Signature (Sign var ty)) = Just ty
  getVal (FunDecl _ _ _) = Nothing
  getVal (TmpVarDecl _ _) = Nothing

-- | Check type correctness of a given expression and initial type environment
staticTypeCheck :: Exp -> Env Type -> Err Type
staticTypeCheck exp env = runReaderT (checkType exp) env

-- | Evaluate type of expression and check equality
compareType :: Type -> Exp -> TypeM Type
compareType t e = checkType e >>= simpleCheck t

compareTypes :: [Type] -> [Exp] -> TypeM ()
compareTypes ts es = mapM_ (uncurry compareType) $ zip ts es

-- | Simple type comparison
simpleCheck :: Type -> Type -> TypeM Type
simpleCheck tExpected tActual = if tExpected == tActual
    then return tActual
    else fail $ "TypeCheckError: Expected " ++ show tExpected ++ ", got " ++ show tActual

-- | Check if type of all expressions is equal
sameTypes :: [Exp] -> TypeM Type
sameTypes (e:es) = do
  t <- checkType e
  mapM_ (compareType t) es
  return t

checkType :: Exp -> TypeM Type
checkType (If e1 e2 e3) = do
  compareType boolType e1
  sameTypes [e2, e3]

checkType (OAdd e1 e2) = compareTypes [intType, intType] [e1, e2] >> return intType
checkType (OSub e1 e2) = compareTypes [intType, intType] [e1, e2] >> return intType
checkType (OMul e1 e2) = compareTypes [intType, intType] [e1, e2] >> return intType
checkType (ODiv e1 e2) = compareTypes [intType, intType] [e1, e2] >> return intType

checkType (ONeg e1) = compareType intType e1 >> return intType

checkType (EOpE e1 compOp e2) = compareTypes [intType, intType] [e1, e2] >> return boolType

checkType (OAnd e1 e2) = compareTypes [boolType, boolType] [e1, e2] >> return boolType
checkType (OOr e1 e2) = compareTypes [boolType, boolType] [e1, e2] >> return boolType

-- | Get 'static expression' from environment and evaluate it.
checkType (VarExp var) = do
  sType <- asks $ lookupVar var
  case sType of
    Ok (t, env) -> return t
    Bad err -> fail err

-- | Full or partial application of lambda expression 'e1' to 'e2'
checkType (FApp e1 e2) = do
  t1 <- checkType e1
  case t1 of
    FunType t2 t3 -> compareType t2 e2 >> return t3
    _ -> fail "TypeCheckError: Only function can be applied to an argument."

checkType (Lambda ((Sign v t):signs) exp) = do
  env <- ask
  let recExp = if signs == [] then exp else Lambda signs exp
  recT <- local (assignStaticVal v (t, env)) $ checkType recExp
  return $ FunType t recT

checkType (Let decls e) = do {
  env <- ask;
  nEnv <- case evalDecls decls env of {
    Ok newEnv -> return newEnv;
    Bad err -> fail err
  };
  local (const nEnv) $ mapM_ checkDeclType decls;
  local (const nEnv) $ checkType e
}

checkType (LitExp lit) = return $ GTyCon $ SimpleTyCon $ ConTyCon $ Con $ case lit of
  IntLit _ -> "Int"
  DoubleLit _ -> "Double"
  CharLit _ -> "Char"
  StringLit _ -> "String"

checkType (TupleExp e es) = do
  t <- checkType e
  ts <- mapM checkType es
  return $ TupleType t ts

checkType (ListExp es) = liftM ListType $ sameTypes es

-- TODO
-- checkType (Case exp alts) = do
--     -- Try to match expression against patterns one by one.
--     (exp, env) <- asum $ map (tryMatch exp) alts
--     -- Evaluate matched 'static expresion'
--     local (const env) $ evalExp exp
--   where
--     tryMatch :: Exp -> Alt -> ExpM StaticExp
--     tryMatch e1 (Alt pat e2) = do
--       env <- ask
--       lEnv <- matchAgainst pat e1 Map.empty
--       return (e2, expandEnv lEnv env)

-------- TODO: not implemented yet

checkType (GConExp gCon) = fail $ "TypeCheckError: Undefined case: GCon"
checkType x = fail $ "TypeCheckError: Undefined case: " ++ show x


------------------- Declaration types ----------------------
checkDeclType :: Decl -> TypeM ()
checkDeclType (Signature _) = return ()
checkDeclType (TmpVarDecl var exp) = do
  t1 <- checkType $ VarExp var
  t2 <- checkType exp
  simpleCheck t1 t2
  return ()

-- TODO
checkDeclType (FunDecl _ _ _) = fail $ "TypeCheckError: Undefined FunDecl"
