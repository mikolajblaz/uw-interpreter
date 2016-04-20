module InterpretBody where

import Control.Monad.State
import Data.List
import Data.Map
import Data.Maybe

import AbsMbCore
import ErrM

-- TODO
--type DataEnv = Map Con DataDecl
type DataEnv = String


failure :: Show a => a -> Err String
failure x = Bad $ "Undefined case: " ++ show x

-- | Main interpreting function
interpretBody :: Body -> Err String
interpretBody (Body topdecls) = do
    dataEnv <- buildDataEnv dataDecls
    interpretDecls decls dataEnv
  where
    (dataDecls, decls) = Data.List.partition isDataDecl topdecls
    isDataDecl (DataDecl _) = True
    isDataDecl _  = False

buildDataEnv :: [TopDecl] -> Err DataEnv
buildDataEnv [] = Ok "Empty DataEnv"
buildDataEnv x = failure x

interpretDecls :: [TopDecl] -> DataEnv -> Err String
interpretDecls decls _ = interpretTmpVarDecls $ chooseVarDecls decls
  where
    chooseVarDecls ((Decl vd@(TmpVarDecl _ _)):ds) = (vd : chooseVarDecls ds)
    chooseVarDecls (_:ds) = chooseVarDecls ds
    chooseVarDecls [] = []

interpretTmpVarDecls :: [Decl] -> Err String
interpretTmpVarDecls vds = do
  finalState <- execStateT (evalDecls vds) empty
  return $ show finalState


-- | A monad to work in
type VarEnv = Map Var Integer
type M = StateT VarEnv Err


evalDecls :: [Decl] -> M ()
evalDecls [] = return ()
evalDecls ((TmpVarDecl var e):ds) = do
  n <- evalExp e
  setStateValue var n
  evalDecls ds


lookupVar name var_map = fromJust $ Data.Map.lookup name var_map
setValue var val var_map = Data.Map.insert var val var_map
setStateValue var val = modify (setValue var val)


evalExp :: Exp -> M Integer
evalExp (Let ((TmpVarDecl var e1):_) e2) = do {
	n1 <- evalExp e1;
  setStateValue var n1;
	evalExp e2;
}

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

evalExp (VarExp var) = gets $ lookupVar var
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
