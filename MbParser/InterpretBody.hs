module InterpretBody where

import Control.Monad.State
import Data.List
import Data.Map
import Data.Maybe

import AbsMbCore
import ErrM

-- TODO
type DataEnv = Map Con DataDecl


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
interpretTmpVarDecls vds = Ok $ show vds


-- | A monad to work in
type VarEnv = Map Var Int
type M = StateT VarEnv Err


evalExp :: Decl -> M Int
evalExp e = execStateT (processExp e) empty

lookupVar (Var name) var_map = fromJust $ Data.Map.lookup name var_map
setValue (Var var) val var_map = Data.Map.insert var val var_map
setStateValue (Var var) val = modify (setValue (Var var) val)


processExp :: Exp -> M Int
processExp (Let (TmpVarDecl var (e1:_)) e2) = do {
	n1 <- processExp e1;
  setStateValue var n1;
	processExp e2;
}

processExp (If e1 e2 e3) = do {
	n1 <- processExp e1;
  processExp $ if n1 == 1 then e2 else e3;
}

processExp (OAdd e1 e2) = binOp e1 e2 (+)
processExp (OSub e1 e2) = binOp e1 e2 (-)
processExp (OMul e1 e2) = binOp e1 e2 (*)
processExp (ODiv e1 e2) = binOp e1 e2 (/)
processExp (ONeg e1) = do
  n1 <- processExp e1
  return (-n1)

processExp (EOpE e1 compOp e2) = do {
	n1 <- processExp e1;
	n2 <- processExp e2;
	return $ if (n1 `evalOp` n2) then 1 else 0;
} where evalOp = case compOp of {
  OEq  -> (==);
  ONeq -> (/=);
	OLt  -> (<);
	OLte -> (<=);
	OGt  -> (>);
  OGte -> (>=);
}

processExp (OAnd e1 e2) = binOp e1 e2 (*)
processExp (OOr e1 e2) = binOp e1 e2 (\x y -> (x + y + 1) `div` 2)

processExp (FExp (AExp aexp)) = processAExp aexp

binOp :: Exp -> Exp -> (Int -> Int -> Int) -> M Int
binOp e1 e2 op = do
  n1 <- processExp e1
  n2 <- processExp e2
  return $ n1 `op` n2



processAExp :: Exp -> M Int
processAExp (VarExp var) = gets $ lookupVar var
processAExp (LitExp (IntLit int)) = return int
processAExp (ParExp e) = processExp e

processAExp (GConExp gCon) = undefined
processAExp (TupleExp e1 es) = undefined
processAExp (ListExp es) = undefined
