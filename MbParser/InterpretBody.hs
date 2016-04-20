module InterpretBody where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.List ( partition )
import qualified Data.Map as Map
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
  finalState <- execStateT (evalDecls vds) Map.empty
  return $ show finalState


evalDecls :: [Decl] -> M ()
evalDecls [] = return ()
evalDecls ((TmpVarDecl var e):ds) = do
  n <- evalExp e
  setStateValue var n
  evalDecls ds

newtype Binding = Binding (Map.Map Var ([Decl], Maybe Binding))

-- | Extract variable from declaration
-- TODO: maybe extract many vars, if declaration is "Pattern = Exp"
getVar :: Decl -> Var
getVar (FunDecl var _ _) = var
getVar (TmpVarDecl var _) = var
getVar (Signature var ty) = undefined

-- | Turn list of declarations to a map, where each varibale has its own
-- declarations as value
collectLocalDecl :: [Decl] -> Map.Map Var [Decl]
collectLocalDecl decls = foldr addDeclToList Map.empty decls
  where
    addDeclToList :: Decl -> Map.Map Var [Decl] -> Map.Map Var [Decl]
    addDeclToList d = Map.alter (appendDecl d) (getVar d)
    appendDecl :: Decl -> Maybe [Decl] -> Maybe [Decl]
    appendDecl d Nothing = Just [d]
    appendDecl d (Just ds) = Just (d:ds)

-- | Insert declarations (of 1 variable) from local scope to binding
-- from outer scope.
-- Simply replaces the old value.
insertDeclToBinding :: Var -> [Decl] -> Binding -> Binding
insertDeclToBinding var decls = Map.insert var (decls, Nothing)

-- | Insert all declarations from local scope to binding from outer scope.
insertLocalToOuter :: [Decl] -> Binding -> Binding
insertLocalToOuter localDecl binding =
  Map.foldrWithKey insertDeclToBinding binding $ collectLocalDecl localDecl


-- | A monad to work in
type M = ReaderT Binding Err

lookupVar name var_map = e
  where ([TmpVarDecl var e], Just binding) = fromJust $ Map.lookup name var_map

setValue var val var_map = Map.insert var val var_map
setStateValue var val = modify (setValue var val)


evalExp :: Exp -> M Integer
evalExp (Let decls e) = local (insertLocalToOuter decls) $ evalExp e

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

evalExp (VarExp var) = asks $ lookupVar var
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
