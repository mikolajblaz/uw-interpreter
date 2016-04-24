module InterpretBody where

import Control.Monad.Reader
import qualified Data.List ( partition )
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment

failure :: Show a => a -> Err String
failure x = Bad $ "Undefined case: " ++ show x

-- | Main interpreting function
interpretBody :: Body -> Err String
interpretBody (Body topdecls) = do
    -- dataEnv <- buildDataEnv dataDecls
    interpretTopDecls decls -- dataEnv
  where
    (dataDecls, decls) = Data.List.partition isDataDecl topdecls
    isDataDecl (DataDecl _) = True
    isDataDecl _  = False


----------------------- Types -----------------------

-- buildDataEnv :: [TopDecl] -> Err DataEnv
-- buildDataEnv [] = Ok "Empty DataEnv"
-- buildDataEnv x = failure x

----------------------- Declarations -----------------------

interpretTopDecls :: [TopDecl] -> Err String
interpretTopDecls decls = interpretTmpVarDecls $ chooseVarDecls decls
  where
    chooseVarDecls ((Decl d):ds) = (d : chooseVarDecls ds)
    chooseVarDecls [] = []

interpretTmpVarDecls :: [Decl] -> Err String
interpretTmpVarDecls decls = let rootExp = Let decls (VarExp (Var "main")) in do
  finalVal <- runReaderT (evalExp rootExp) $ Env Map.empty
  return $ show finalVal


evalDecls :: [Decl] -> Env -> Env
evalDecls = localToOuterEnv


----------------------- Expressions -----------------------

evalExp :: Exp -> M Integer
evalExp (Let decls e) = local (evalDecls decls) $ evalExp e

evalExp (If e1 e2 e3) = do {
  n1 <- evalExp e1;
  evalExp $ if n1 == 1 then e2 else e3;
}

evalExp (OAdd e1 e2) = binOp e1 e2 (+)
evalExp (OSub e1 e2) = binOp e1 e2 (-)
evalExp (OMul e1 e2) = binOp e1 e2 (*)
evalExp (ODiv e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  if n2 /= 0
    then return $ n1 `div` n2
    else fail "Error: Division by 0"

evalExp (ONeg e1) = do
  n1 <- evalExp e1
  return (-n1)

evalExp (EOpE e1 compOp e2) = do {
  n1 <- evalExp e1;
  n2 <- evalExp e2;
  return $ if (n1 `evalOp` n2) then 1 else 0;
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

-- TODO
evalExp (VarExp var) = do {
  (e, oEnv, lEnv) <- asks $ lookupVar var;
  local (const oEnv) $ evalExp e
  -- TODO: include lEnv
}

evalExp (LitExp (IntLit int)) = return int

evalExp (FApp e1 e2) = undefined
evalExp (GConExp gCon) = undefined
evalExp (TupleExp e1 es) = undefined
evalExp (ListExp es) = undefined

binOp :: Exp -> Exp -> (Integer -> Integer -> Integer) -> M Integer
binOp e1 e2 op = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return $ n1 `op` n2
