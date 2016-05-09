module Expressions where

import Control.Monad.Reader
import Data.Foldable ( asum )
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment
type StaticExp = StaticVal Exp
type ExpM = EvalM Exp

instance EnvVal Exp where
  getVal (Signature (Sign var ty)) = undefined
  getVal (FunDecl _ _ _) = undefined
  getVal (TmpVarDecl _ exp) = exp


evalExpVal :: Exp -> ExpM Exp
evalExpVal e = liftM fst (evalExp e)

emptyEnv :: ExpM Exp -> ExpM StaticExp
emptyEnv me = liftM (\e -> (e, Env Map.empty Map.empty)) me


runExp :: Exp -> Env Exp -> Err Exp
runExp exp env = liftM fst $ runReaderT (evalExp exp) env

-- | Evaluate expression in an environment hidden in 'Reader' monad
evalExp :: Exp -> ExpM StaticExp
-- | Extend environment by evaluating let declarations
evalExp (Let decls e) = do {
  env <- ask;
  case evalDecls decls env of
    Ok newEnv -> local (const newEnv) $ evalExp e
    Bad err -> fail err
}

evalExp (If e1 e2 e3) = do {
  n1 <- evalExpVal e1;
  evalExp $ if n1 == LitExp (IntLit 1) then e2 else e3;
}

evalExp (OAdd e1 e2) = emptyEnv $ binOp e1 e2 (+)
evalExp (OSub e1 e2) = emptyEnv $ binOp e1 e2 (-)
evalExp (OMul e1 e2) = emptyEnv $ binOp e1 e2 (*)
evalExp (ODiv e1 e2) = emptyEnv $ do
  LitExp (IntLit n1) <- evalExpVal e1
  LitExp (IntLit n2) <- evalExpVal e2
  if n2 /= 0
    then return $ LitExp (IntLit (n1 `div` n2))
    else fail "Error: Division by 0"

evalExp (ONeg e1) = emptyEnv $ do
  LitExp (IntLit n) <- evalExpVal e1
  return $ LitExp (IntLit (-n))

evalExp (EOpE e1 compOp e2) = emptyEnv $ do {
  n1 <- evalExpVal e1;
  n2 <- evalExpVal e2;
  return $ if (n1 `evalOp` n2) then LitExp (IntLit 1) else LitExp (IntLit 0);
} where evalOp = case compOp of {
  OEq  -> (==);
  ONeq -> (/=);
  OLt  -> (<);
  OLte -> (<=);
  OGt  -> (>);
  OGte -> (>=);
}

evalExp (OAnd e1 e2) = emptyEnv $ binOp e1 e2 (*)
evalExp (OOr e1 e2) = emptyEnv $ binOp e1 e2 (\x y -> (x + y + 1) `div` 2)

-- | Get 'static expression' from environment and evaluate it.
evalExp (VarExp var) = do {
  sExp <- asks $ lookupVar var;
  case sExp of
    Ok (e, env) -> local (const env) $ evalExp e
    Bad err -> fail err
}

-- | Full or partial application of lambda expression 'e1' to 'e2'
evalExp (FApp e1 e2) = do
  env <- ask;
  (ev1, env1) <- evalExp e1
  let sExp2 = (e2, env)
  case ev1 of
    Lambda ((Sign v t):[]) exp -> let eEnv1 = assignStaticVal v sExp2 env1 in
      local (const eEnv1) $ evalExp exp
    Lambda ((Sign v t):vars) exp -> let eEnv1 = assignStaticVal v sExp2 env1 in
      return (Lambda vars exp, eEnv1)

-- | Not evaluating if not needed
evalExp lam@(Lambda _ _) = emptyEnv $ return lam
evalExp lit@(LitExp _) = emptyEnv $ return lit
evalExp tup@(TupleExp _ _) = emptyEnv $ return tup
evalExp lst@(ListExp _) = emptyEnv $ return lst

evalExp (Case exp alts) = do
    -- Try to match expression against patterns one by one.
    (exp, env) <- asum $ map (tryMatch exp) alts
    -- Evaluate matched 'static expresion'
    local (const env) $ evalExp exp
  where
    tryMatch :: Exp -> Alt -> ExpM StaticExp
    tryMatch e1 (Alt pat e2) = do
      env <- ask
      lEnv <- matchAgainst pat e1 Map.empty
      return (e2, expandEnv lEnv env)

-------- TODO: not implemented yet

evalExp (GConExp gCon) = undefined

binOp :: Exp -> Exp -> (Integer -> Integer -> Integer) -> ExpM Exp
binOp e1 e2 op = do
  LitExp (IntLit n1) <- evalExpVal e1
  LitExp (IntLit n2) <- evalExpVal e2
  return $ LitExp (IntLit (n1 `op` n2))



------------- Pattern matching -------------------
matchAgainst :: Pat -> Exp -> LocalEnv Exp -> ExpM (LocalEnv Exp)
matchAgainst pat exp lEnv = do
  (evaledExp, env) <- evalExp exp
  case (pat, evaledExp) of
    (VarPat var, e) -> case setLocalVar var e lEnv of
      Ok newEnv -> return newEnv
      Bad err -> fail err
    (WildCard, e) -> return lEnv
    (LitPat _, l) -> return lEnv -- TODO: assuming type check, TODO: check equality
    (ListPat ps, ListExp es) -> foldM (flip . uncurry $ matchAgainst) lEnv $ zip ps es
    (TuplePat p ps, TupleExp e es) -> foldM (flip . uncurry $ matchAgainst) lEnv $ zip (p:ps) (e:es)
    _ -> mzero  -- TODO
