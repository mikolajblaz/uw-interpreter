module Expressions where

import Control.Monad.Reader
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment


runExp :: Exp -> Env -> Err Exp
runExp exp env = runReaderT (evalExp exp) env

evalExp :: Exp -> EvalM Exp
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

evalExp (VarExp var) = do {
  sExp <- asks $ lookupVar var;
  case sExp of
    Ok (e, env) -> local (const env) $ evalExp e
    Bad err -> fail err
}

evalExp (FApp e1 e2) = do
  n1 <- evalExp e1
  case n1 of
    Lambda (v:[]) exp -> evalExp (Let [TmpVarDecl v e2] exp)
    Lambda (v:vars) exp -> evalExp (Let [TmpVarDecl v e2] (Lambda vars exp))
evalExp lam@(Lambda _ _) = return lam
evalExp lit@(LitExp _) = return lit

-------- TODO: not implemented yet
evalExp (Case exp alts) = undefined

evalExp (GConExp gCon) = undefined
evalExp (TupleExp e1 es) = undefined
evalExp (ListExp es) = undefined

binOp :: Exp -> Exp -> (Integer -> Integer -> Integer) -> EvalM Exp
binOp e1 e2 op = do
  LitExp (IntLit n1) <- evalExp e1
  LitExp (IntLit n2) <- evalExp e2
  return $ LitExp (IntLit (n1 `op` n2))
