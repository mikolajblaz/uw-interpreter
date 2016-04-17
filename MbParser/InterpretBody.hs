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
  finalState <- execStateT (processDecls vds) empty
  return $ show finalState


-- | A monad to work in
type VarEnv = Map Var Integer
type M = StateT VarEnv Err


processDecls :: [Decl] -> M ()
processDecls [] = return ()
processDecls ((TmpVarDecl var e):ds) = do
  n <- processExp e
  setStateValue var n
  processDecls ds


lookupVar name var_map = fromJust $ Data.Map.lookup name var_map
setValue var val var_map = Data.Map.insert var val var_map
setStateValue var val = modify (setValue var val)


processExp :: Exp -> M Integer
processExp (Let ((TmpVarDecl var e1):_) e2) = do {
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
processExp (ODiv e1 e2) = do
  n1 <- processExp e1
  n2 <- processExp e2
  if n2 /= 0
    then return $ n1 `div` n2
    else fail "Error: Division by 0"

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

processExp (VarExp var) = gets $ lookupVar var
processExp (LitExp (IntLit int)) = return int

processExp (FApp e1 e2) = undefined
processExp (GConExp gCon) = undefined
processExp (TupleExp e1 es) = undefined
processExp (ListExp es) = undefined

binOp :: Exp -> Exp -> (Integer -> Integer -> Integer) -> M Integer
binOp e1 e2 op = do
  n1 <- processExp e1
  n2 <- processExp e2
  return $ n1 `op` n2
