module Types where

import Control.Monad.Reader
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment

----------------- Static type check ---------------------------
type StaticType = StaticVal Type
type TypeM = EvalM Type

-- TODO: remove all but Bool and Int maybe
boolType, intType, doubleType, charType, stringType :: Type
boolType = TyCon $ Con "Bool"
intType = TyCon $ Con "Int"
doubleType = TyCon $ Con "Double"
charType = TyCon $ Con "Char"
stringType = TyCon $ Con "String" -- TODO: maybe list of chars

instance EnvVal Type where
  getVal (Signature (Sign var ty)) = Just ty
  getVal (VarDecl _ _) = Nothing

-- | Check type correctness of a given expression and initial type environment
staticTypeCheck :: Exp -> Env Type -> DataEnv -> Err Type
staticTypeCheck exp env de = runReaderT (checkType de exp) env

-- | Evaluate type of expression and check equality
compareType :: DataEnv -> Type -> Exp -> TypeM Type
compareType de t e = checkType de e >>= simpleCheck t

compareTypes :: DataEnv -> [Type] -> [Exp] -> TypeM ()
compareTypes de ts es = mapM_ (uncurry $ compareType de) $ zip ts es

-- | Simple type comparison
simpleCheck :: Type -> Type -> TypeM Type
simpleCheck tExpected tActual = if tExpected == tActual
    then return tActual
    else fail $ "TypeCheckError: Expected " ++ show tExpected ++ ", got " ++ show tActual

-- | Check if type of all expressions is equal
sameTypes :: DataEnv -> [Exp] -> TypeM Type
sameTypes de (e:es) = do
  t <- checkType de e
  mapM_ (compareType de t) es
  return t

checkType :: DataEnv -> Exp -> TypeM Type
checkType de (If e1 e2 e3) = do
  compareType de boolType e1
  sameTypes de [e2, e3]

checkType de (OAdd e1 e2) = compareTypes de [intType, intType] [e1, e2] >> return intType
checkType de (OSub e1 e2) = compareTypes de [intType, intType] [e1, e2] >> return intType
checkType de (OMul e1 e2) = compareTypes de [intType, intType] [e1, e2] >> return intType
checkType de (ODiv e1 e2) = compareTypes de [intType, intType] [e1, e2] >> return intType

checkType de (ONeg e1) = compareType de intType e1 >> return intType

checkType de (EOpE e1 compOp e2) = compareTypes de [intType, intType] [e1, e2] >> return boolType

checkType de (OAnd e1 e2) = compareTypes de [boolType, boolType] [e1, e2] >> return boolType
checkType de (OOr e1 e2) = compareTypes de [boolType, boolType] [e1, e2] >> return boolType

-- | Read variable type from environment
checkType _ (VarExp var) = do
  sType <- asks $ lookupVar var
  case sType of
    Ok (t, env) -> return t
    Bad err -> fail err

checkType de (FApp e1 e2) = do
  t1 <- checkType de e1
  case t1 of
    FunType t2 t3 -> compareType de t2 e2 >> return t3
    _ -> fail "TypeCheckError: Only function can be applied to an argument."

checkType de (Lambda ((Sign v t):signs) exp) = do
  env <- ask
  let recExp = if signs == [] then exp else Lambda signs exp
  recT <- local (assignStaticVal v (t, env)) $ checkType de recExp
  return $ FunType t recT

checkType de (Let decls e) = do {
  env <- ask;
  nEnv <- case evalDecls decls env of {
    Ok newEnv -> return newEnv;
    Bad err -> fail err
  };
  let Env oEnv lEnv = nEnv in
  --fail $ show lEnv;
  -- check if type of declaration matches signature (stored in nEnv)
  mapM_ ((local (const nEnv)) . checkDeclType de) decls;
  local (const nEnv) $ checkType de e
}

checkType _ (LitExp lit) = return $ TyCon $ Con $ case lit of
  IntLit _ -> "Int"
  DoubleLit _ -> "Double"
  CharLit _ -> "Char"
  StringLit _ -> "String"

checkType de (TupleExp e es) = do
  t <- checkType de e
  ts <- mapM (checkType de) es
  return $ TupleType t ts

checkType de (ListExp es) = liftM ListType $ sameTypes de es

checkType de (Case e alts) = do
  t <- checkType de e
  -- get types of all expressions
  (t:ts) <- mapM (checkAltType de t) alts
  -- check if their type is the same as the type of first one
  mapM_ (simpleCheck t) ts
  return t

checkType de (ConExp con) = case conToType con de of
  Ok t -> return t
  Bad err -> fail err


--------------------- Pattern types ------------------------
-- | Check if pattern has the given type and return type of matched expression
checkAltType :: DataEnv -> Type -> Alt -> TypeM Type
checkAltType de t (Alt pat e) = do
  env <- ask
  -- check type of pattern and expand type environment
  newEnv <- case matchAgainstType de t pat Map.empty of
    Ok lEnv -> return $ expandEnv lEnv env
    Bad err -> fail err
  -- check type of expression in a new environment
  local (const newEnv) $ checkType de e

-- | Match type against pattern and change local environment
matchAgainstType :: DataEnv -> Type -> Pat -> LocalEnv Type -> Err (LocalEnv Type)
matchAgainstType de t pat lEnv = do
  let failMsg = "TypeCheckError: type of " ++ show pat ++ " doesn't match expected type " ++ show t
  case (pat, t) of
    (WildCard, _) -> return lEnv
    (VarPat var, t) -> case setLocalVar var t lEnv of
      Ok newEnv -> return newEnv
      Bad err -> fail err
    (LitPat lit, TyCon (Con con)) -> case (lit, con) of
      (IntLit _, "Int") -> return lEnv
      (DoubleLit _, "Double") -> return lEnv
      (CharLit _, "Char") -> return lEnv
      (StringLit _, "String") -> return lEnv -- TODO maybe list of Chars
      _ -> fail failMsg
    (ListPat ps, ListType t) -> foldM (flip $ matchAgainstType de t) lEnv $ ps
    (TuplePat p ps, TupleType t ts) -> if length ps == length ts
      then foldM (flip . uncurry $ matchAgainstType de) lEnv $ zip (t:ts) (p:ps)
      else fail failMsg
    (ConPat con1 ps, TyCon con2) -> do
      (dataCon, ts) <- getConData con1 de
      if dataCon == con2
        then foldM (flip . uncurry $ matchAgainstType de) lEnv $ zip ts ps
        else fail failMsg
    _ -> fail failMsg

------------------- Declaration types ----------------------
-- | Compare type of declared expression with type of (previously
-- stored in environment) signature
checkDeclType :: DataEnv -> Decl -> TypeM ()
checkDeclType _ (Signature _) = return ()
checkDeclType de (VarDecl var exp) = do
  -- expected type of a variable is taken from environment
  t1 <- checkType de $ VarExp var
  t2 <- checkType de exp
  simpleCheck t1 t2
  return ()
