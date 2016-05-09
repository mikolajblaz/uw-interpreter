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
  getVal (Signature (Sign var ty)) = ty
  getVal (FunDecl _ _ _) = undefined
  getVal (TmpVarDecl _ _) = undefined

-- | Check type correctness of a given expression and initial type environment
staticTypeCheck :: Exp -> Env Type -> Err Type
staticTypeCheck exp env = runReaderT (checkType exp) env

-- | Evaluate type of expression and check equality
compareType :: Exp -> Type -> TypeM Type
compareType e t = checkType e >>= equalityCheck t

compareTypes :: [Exp] -> [Type] -> TypeM ()
compareTypes es ts = mapM_ (uncurry compareType) $ zip es ts

-- | Simple type comparison
equalityCheck :: Type -> Type -> TypeM Type
equalityCheck tExpected tActual = if tExpected == tActual
    then return tActual
    else fail $ "Expected " ++ show tExpected ++ ", got " ++ show tActual

checkType :: Exp -> TypeM Type
-- checkType _ = return $ GTyCon UnitTyCon
checkType (If e1 e2 e3) = do
  compareType e1 boolType
  t2 <- checkType e2
  t3 <- checkType e3
  if (t2 == t3)
    then return t2
    else fail "Both \"If\" branches should have the same type"

checkType (OAdd e1 e2) = compareTypes [e1, e2] [intType, intType] >> return intType
checkType (OSub e1 e2) = compareTypes [e1, e2] [intType, intType] >> return intType
checkType (OMul e1 e2) = compareTypes [e1, e2] [intType, intType] >> return intType
checkType (ODiv e1 e2) = compareTypes [e1, e2] [intType, intType] >> return intType

checkType (ONeg e1) = compareType e1 intType >> return intType

checkType (EOpE e1 compOp e2) = compareTypes [e1, e2] [intType, intType] >> return boolType

checkType (OAnd e1 e2) = compareTypes [e1, e2] [boolType, boolType] >> return boolType
checkType (OOr e1 e2) = compareTypes [e1, e2] [boolType, boolType] >> return boolType

-- | Get 'static expression' from environment and evaluate it.
checkType (VarExp var) = do
  (t, st) <- asks $ lookupVar var
  return t

-- | Full or partial application of lambda expression 'e1' to 'e2'
checkType (FApp e1 e2) = do
  t1 <- checkType e1
  case t1 of
    FunType t2 t3 -> compareType e2 t2 >> return t3
    _ -> fail "Only function can be applied to an argument."

-- TODO
-- checkType lam@(Lambda _ _) = emptyEnv $ return lam
-- checkType (Let decls e) = do {
--   env <- ask;
--   case evalDecls decls env of
--     Ok newEnv -> local (const newEnv) $ evalExp e
--     Bad err -> fail err
-- }

checkType (LitExp lit) = return $ GTyCon $ SimpleTyCon $ ConTyCon $ Con $ case lit of
  IntLit _ -> "Int"
  DoubleLit _ -> "Double"
  CharLit _ -> "Char"
  StringLit _ -> "String"

checkType (TupleExp e es) = do
  t <- checkType e
  ts <- mapM checkType es
  return $ TupleType t ts

-- TODO
-- checkType (ListExp es) = mapM checkType es >>= return . ListType

-- TODO
-- evalExp (Case exp alts) = do
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

evalExp (GConExp gCon) = undefined
