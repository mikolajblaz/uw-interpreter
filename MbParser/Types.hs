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
stringType = GTyCon $ SimpleTyCon $ ConTyCon $ Con "String" -- TODO: maybe list of chars

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
  let Env oEnv lEnv = nEnv in
  --fail $ show lEnv;
  -- check if type of declaration matches signature (stored in nEnv)
  mapM_ ((local (const nEnv)) . checkDeclType) decls;
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

checkType (Case e alts) = do
  t <- checkType e
  -- get types of all expressions
  (t:ts) <- mapM (checkAltType t) alts
  -- check if their type is the same as the type of first one
  mapM_ (simpleCheck t) ts
  return t

-------- TODO: not implemented yet

checkType (GConExp gCon) = fail $ "TypeCheckError: Undefined case: GCon"
checkType x = fail $ "TypeCheckError: Undefined case: " ++ show x


--------------------- Pattern types ------------------------
-- | Check if pattern has the given type and return type of matched expression
checkAltType :: Type -> Alt -> TypeM Type
checkAltType t (Alt pat e) = do
  env <- ask
  -- check type of pattern and expand type environment
  newEnv <- case matchAgainstType t pat Map.empty of
    Ok lEnv -> return $ expandEnv lEnv env
    Bad err -> fail err
  -- check type of expression in a new environment
  local (const newEnv) $ checkType e

-- | Match type against pattern and change local environment
matchAgainstType :: Type -> Pat -> LocalEnv Type -> Err (LocalEnv Type)
matchAgainstType t pat lEnv = do
  let failMsg = "TypeCheckError: type of " ++ show pat ++ " doesn't match expected type " ++ show t
  case (pat, t) of
    (WildCard, _) -> return lEnv
    (VarPat var, t) -> case setLocalVar var t lEnv of
      Ok newEnv -> return newEnv
      Bad err -> fail err
    (LitPat lit, GTyCon (SimpleTyCon (ConTyCon (Con con)))) -> case (lit, con) of
      (IntLit _, "Int") -> return lEnv
      (DoubleLit _, "Double") -> return lEnv
      (CharLit _, "Char") -> return lEnv
      (StringLit _, "String") -> return lEnv -- TODO maybe list of Chars
      _ -> fail failMsg
    (ListPat ps, ListType t) -> foldM (flip $ matchAgainstType t) lEnv $ ps
    (TuplePat p ps, TupleType t ts) -> if length ps == length ts
      then foldM (flip . uncurry $ matchAgainstType) lEnv $ zip (t:ts) (p:ps)
      else fail failMsg
    _ -> fail failMsg  -- TODO

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
