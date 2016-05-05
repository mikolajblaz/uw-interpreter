module Expressions where

import Control.Monad.Reader
import Data.Foldable ( asum )
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment


runExp :: Exp -> Env -> Err Exp
runExp exp env = runReaderT (evalExp exp) env

-- | Evaluate expression in an environment hidden in 'Reader' monad
evalExp :: Exp -> EvalM Exp
-- | Extend environment by evaluating let declarations
evalExp (Let decls e) = do {
  env <- ask;
  case evalDecls decls env of
    Ok newEnv -> local (const newEnv) $ evalExp e
    Bad err -> fail err
}

evalExp (If e1 e2 e3) = do {
  n1 <- evalExp e1;
  evalExp $ if n1 == LitExp (IntLit 1) then e2 else e3;
}

evalExp (OAdd e1 e2) = binOp e1 e2 (+)
evalExp (OSub e1 e2) = binOp e1 e2 (-)
evalExp (OMul e1 e2) = binOp e1 e2 (*)
evalExp (ODiv e1 e2) = do
  LitExp (IntLit n1) <- evalExp e1
  LitExp (IntLit n2) <- evalExp e2
  if n2 /= 0
    then return $ LitExp (IntLit (n1 `div` n2))
    else fail "Error: Division by 0"

evalExp (ONeg e1) = do
  LitExp (IntLit n) <- evalExp e1
  return $ LitExp (IntLit (-n))

evalExp (EOpE e1 compOp e2) = do {
  n1 <- evalExp e1;
  n2 <- evalExp e2;
  return $ if (n1 `evalOp` n2) then LitExp (IntLit 1) else LitExp (IntLit 0);
} where evalOp = case compOp of {
  OEq  -> (==);
  ONeq -> (/=);
  OLt  -> (<);
  OLte -> (<=);
  OGt  -> (>);
  OGte -> (>=);
}

evalExp (OAnd e1 e2) = binOp e1 e2 (*)
evalExp (OOr e1 e2) = binOp e1 e2 (\x y -> (x + y + 1) `div` 2)

-- | Get 'static expression' from environment and evaluate it.
evalExp (VarExp var) = do {
  sExp <- asks $ lookupVar var;
  case sExp of
    Ok (e, env) -> local (const env) $ evalExp e
    Bad err -> fail err
}

-- | Full or partial application of lambda expression 'e1' to 'e2'
evalExp (FApp e1 e2) = do
  n1 <- evalExp e1
  case n1 of
    -- TODO: WRONG! not static binding
    Lambda (v:[]) exp -> evalExp (Let [TmpVarDecl v e2] exp)
    Lambda (v:vars) exp -> evalExp (Let [TmpVarDecl v e2] (Lambda vars exp))

-- | Not evaluating if not needed
evalExp lam@(Lambda _ _) = return lam
evalExp lit@(LitExp _) = return lit
evalExp tup@(TupleExp _ _) = return tup
evalExp lst@(ListExp _) = return lst

evalExp (Case exp alts) = do
    -- Try to match expression against patterns one by one.
    (exp, env) <- asum $ map (tryMatch exp) alts
    -- Evaluate matched 'static expresion'
    local (const env) $ evalExp exp
  where
    tryMatch :: Exp -> Alt -> EvalM StaticExp
    tryMatch e1 (Alt pat e2) = do
      env <- ask
      lEnv <- matchAgainst pat e1 Map.empty
      return (e2, expandEnv lEnv env)

-------- TODO: not implemented yet

evalExp (GConExp gCon) = undefined

binOp :: Exp -> Exp -> (Integer -> Integer -> Integer) -> EvalM Exp
binOp e1 e2 op = do
  LitExp (IntLit n1) <- evalExp e1
  LitExp (IntLit n2) <- evalExp e2
  return $ LitExp (IntLit (n1 `op` n2))



------------- Pattern matching -------------------
matchAgainst :: Pat -> Exp -> LocalEnv -> EvalM LocalEnv
matchAgainst pat exp lEnv = do
  evaledExp <- evalExp exp
  case (pat, evaledExp) of
    (VarPat var, e) -> case setLocalVar var e lEnv of
      Ok newEnv -> return newEnv
      Bad err -> fail err
    (WildCard, e) -> return lEnv
    (LitPat _, l) -> return lEnv -- TODO: assuming type check, TODO: check equality
    (ListPat ps, ListExp es) -> foldM (flip . uncurry $ matchAgainst) lEnv $ zip ps es
    (TuplePat p ps, TupleExp e es) -> foldM (flip . uncurry $ matchAgainst) lEnv $ zip (p:ps) (e:es)
    _ -> mzero  -- TODO









------------- Partial evaluation ---------------------

-- | Evaluate expression in an environment hidden in 'Reader' monad
partEvalExp :: Exp -> EvalM StaticExp
-- | Extend environment by evaluating let declarations
partEvalExp (Let decls e) = do {
  env <- ask;
  case evalDecls decls env of
    Ok newEnv -> local (const newEnv) $ partEvalExp e
    Bad err -> fail err
}

partEvalExp (If e1 e2 e3) = do {
  env <- ask;
  n1 <- evalExp e1;
  partEvalExp $ if n1 == LitExp (IntLit 1) then e2 else e3;
}

partEvalExp (OAdd e1 e2) = do {
  n1 <- evalExp (OAdd e1 e2);
  return $ (n1, Env Map.empty Map.empty)
}
-- partEvalExp (OSub e1 e2) =
-- partEvalExp (OMul e1 e2) =
-- partEvalExp (ODiv e1 e2) =
-- partEvalExp (ONeg e1) =
-- partEvalExp (EOpE e1 compOp e2) =
-- partEvalExp (OAnd e1 e2) =
-- partEvalExp (OOr e1 e2) =

-- | Get 'static expression' from environment and evaluate it.
partEvalExp (VarExp var) = do {
  sExp <- asks $ lookupVar var;
  case sExp of
    Ok (e, env) -> local (const env) $ partEvalExp e
    Bad err -> fail err
}

-- | Full or partial application of lambda expression 'e1' to 'e2'
partEvalExp (FApp e1 e2) = do
  env <- ask;
  (ev1, env1) <- partEvalExp e1
  (ev2, env2) <- partEvalExp e2
  case ev1 of
    Lambda (v:[]) exp -> do {
      -- TODO
      -- wstaw do środowiska env1 mapowanie 'v -> (ev2, env2)'
      -- zwróć (exp, extEnv1)
      partEvalExp (Let [TmpVarDecl v e2] exp)
      --local (evalDecls ([TmpVarDecl v e2])) $ partEvalExp exp
    }
    Lambda (v:vars) exp -> let Ok eEnv = evalDecls ([TmpVarDecl v e2]) env in
      return (Lambda vars exp, eEnv)

-- | Not evaluating if not needed
partEvalExp lam@(Lambda _ _) = do {env <- ask; return (lam, env)}
partEvalExp lit@(LitExp _) = do {env <- ask; return (lit, env)}
partEvalExp tup@(TupleExp _ _) = do {env <- ask; return (tup, env)}
partEvalExp lst@(ListExp _) = do {env <- ask; return (lst, env)}

partEvalExp (Case exp alts) = do
    -- Try to match expression against patterns one by one.
    (exp, env) <- asum $ map (tryMatch exp) alts
    -- Evaluate matched 'static expresion'
    local (const env) $ partEvalExp exp
  where
    tryMatch :: Exp -> Alt -> EvalM StaticExp
    tryMatch e1 (Alt pat e2) = do
      env <- ask
      lEnv <- matchAgainst pat e1 Map.empty
      return (e2, expandEnv lEnv env)

-------- TODO: not implemented yet

partEvalExp (GConExp gCon) = undefined
