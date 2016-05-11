module Expressions where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Applicative
import Data.Foldable ( asum )
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment
type StaticExp = StaticVal Exp
type ExpM = EvalM Exp

-- TODO
-- instance Applicative Err where
--   pure = Ok
--   (Bad s) <*> _ = Bad s
--   (Ok f) <*> o  = liftM f o
--
-- instance Alternative Err where
--   empty = mzero
--   (<|>) = mplus

--------------------- Helper functions --------------------

trueExp, falseExp :: Exp
trueExp = ConExp $ Con "True"
falseExp = ConExp $ Con "False"

instance EnvVal Exp where
  getVal (Signature (Sign var ty)) = Nothing
  getVal (VarDecl _ exp) = Just exp


evalExpVal :: Exp -> ExpM Exp
evalExpVal e = liftM fst (evalExp e)

addEmptyEnv :: ExpM Exp -> ExpM StaticExp
addEmptyEnv me = liftM (\e -> (e, emptyEnv)) me

addEnv :: ExpM Exp -> ExpM StaticExp
addEnv me = do
  env <- ask
  e <- me
  return (e, env)

--------------------- Expressions evaluation ---------------------
-- We evaluate each expression to the point if usability.
-- To ensure laziness we stop evaluation at expressions:
-- lambda, literals, tuples, lists, type constructors
-- (and sometimes function applications, see below)

runExp :: Exp -> Env Exp -> Err Exp
runExp exp env = liftM fst $ runReaderT (evalExp exp) env

-- | Evaluate expression in an environment hidden in 'Reader' monad
evalExp :: Exp -> ExpM StaticExp
-- | Not evaluating if not needed - ensure laziness
evalExp lam@(Lambda _ _) = addEnv $ return lam
evalExp lit@(LitExp _) = addEmptyEnv $ return lit
evalExp tup@(TupleExp _ _) = addEnv $ return tup
evalExp lst@(ListExp _) = addEnv $ return lst
evalExp ce@(ConExp _) = addEmptyEnv $ return ce

-- | Normal evaluation
evalExp (If e1 e2 e3) = do {
  n1 <- evalExpVal e1;
  evalExp $ if n1 == LitExp (IntLit 1) then e2 else e3;
}

evalExp (OAdd e1 e2) = addEmptyEnv $ binOp e1 e2 (+)
evalExp (OSub e1 e2) = addEmptyEnv $ binOp e1 e2 (-)
evalExp (OMul e1 e2) = addEmptyEnv $ binOp e1 e2 (*)
evalExp (ODiv e1 e2) = addEmptyEnv $ do
  LitExp (IntLit n1) <- evalExpVal e1
  LitExp (IntLit n2) <- evalExpVal e2
  if n2 /= 0
    then return $ LitExp (IntLit (n1 `div` n2))
    else fail "Error: Division by 0"

evalExp (ONeg e1) = addEmptyEnv $ do
  LitExp (IntLit n) <- evalExpVal e1
  return $ LitExp (IntLit (-n))

evalExp (EOpE e1 compOp e2) = addEmptyEnv $ do {
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

evalExp (OAnd e1 e2) = addEmptyEnv $ binOp e1 e2 (*)
evalExp (OOr e1 e2) = addEmptyEnv $ binOp e1 e2 (\x y -> (x + y + 1) `div` 2)

-- | Get 'static expression' from environment and evaluate it.
evalExp (VarExp var) = do {
  sExp <- asks $ lookupVar var;
  case sExp of
    Ok (e, env) -> local (const env) $ evalExp e
    Bad err -> fail err
}

-- | Extend environment by evaluating let declarations
evalExp (Let decls e) = do {
env <- ask;
case evalDecls decls env of
  Ok newEnv -> local (const newEnv) $ evalExp e
  Bad err -> fail err
}

-- | Full or partial application of lambda expression 'e1' to 'e2'
evalExp fapp@(FApp e1 e2) = do
  env <- ask;
  (ev1, env1) <- evalExp e1
  let sExp2 = (e2, env)
  case ev1 of
    -- if ev1 is a lambda, it is a normal function application
    Lambda ((Sign v t):[]) exp -> let eEnv1 = assignStaticVal v sExp2 env1 in
      local (const eEnv1) $ evalExp exp
    Lambda ((Sign v t):vars) exp -> let eEnv1 = assignStaticVal v sExp2 env1 in
      return (Lambda vars exp, eEnv1)
    -- if ev1 is a partially constructed type, we leave it as FApp
    ConExp _ -> addEnv $ return fapp
    FApp _ _ -> addEnv $ return fapp

evalExp (Case exp alts) = do
    -- Try to match expression against patterns one by one.
    matches <- mapM (tryMatch exp) alts
    -- Choose first matching pattern (using <|>)
    case asum matches of
      Just (e, env) -> local (const env) $ evalExp e
      Nothing -> fail "RunTimeError: Non-exhaustive patterns in case expression"
  where
    tryMatch :: Exp -> Alt -> ExpM (Maybe StaticExp)
    tryMatch e1 (Alt pat e2) = do
      env <- ask
      lEnv <- matchAgainstExp e1 pat $ Just Map.empty
      case lEnv of
        Just lEnv -> return $ Just (e2, expandEnv lEnv env)
        Nothing -> return Nothing


-- | Binary operations
binOp :: Exp -> Exp -> (Integer -> Integer -> Integer) -> ExpM Exp
binOp e1 e2 op = do
  LitExp (IntLit n1) <- evalExpVal e1
  LitExp (IntLit n2) <- evalExpVal e2
  return $ LitExp (IntLit (n1 `op` n2))



------------- Pattern matching -------------------
-- | Try to match expression against pattern.
-- If pattern is a varibale or wildcard, there is no need to evaluate expression
matchAgainstExp :: Exp -> Pat -> Maybe (LocalEnv Exp) -> ExpM (Maybe (LocalEnv Exp))
matchAgainstExp _ _ Nothing = return Nothing
matchAgainstExp exp pat jLEnv@(Just lEnv) = case pat of
  VarPat var -> case setLocalVar var exp lEnv of
    Ok newEnv -> return $ Just newEnv
    Bad err -> return Nothing
  WildCard -> return jLEnv
  -- otherwise expression must be evaluated
  _ -> do
    (evaledExp, env) <- evalExp exp
    case (pat, evaledExp) of
      (LitPat lp, LitExp le) -> return $ if lp == le then jLEnv else Nothing
      (ListPat ps, ListExp es) -> foldM (flip . uncurry $ matchAgainstExp) jLEnv $ zip es ps
      (TuplePat p ps, TupleExp e es) -> foldM (flip . uncurry $ matchAgainstExp) jLEnv $ zip (e:es) (p:ps)
      (ConPat con1 [], ConExp con2) -> return $ if con1 == con2 then jLEnv else Nothing
      (ConPat con1 [], _) -> return Nothing
      (ConPat con ps, FApp e1 e2) ->
        foldM (flip . uncurry $ matchAgainstExp) jLEnv $ [(e1, ConPat con (init ps)), (e2, last ps)]
      _ -> return Nothing  -- TODO
