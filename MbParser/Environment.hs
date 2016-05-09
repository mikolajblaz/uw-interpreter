module Environment where

import Control.Monad ( foldM )
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

import AbsMbCore
import ErrM

-- | A value that can be kept in an environment
-- Instances: Exp and Type
class EnvVal val where
  -- | Extract value from declaration
  -- Nothing is not an error, it means that value cannot be extracted from
  -- specific type of declarations
  getVal :: Decl -> Maybe val

-- | Whole environment
data Env val = Env (OuterEnv val) (LocalEnv val)
-- | Local environment includes variables introduced by the last 'let'
type LocalEnv val = Map.Map Var val
-- | Outer environment includes other variables
type OuterEnv val = Map.Map Var (StaticVal val)
-- | Value with bound environment
type StaticVal val = (val, Env val)


-- | A monad to evaluate program in
type EvalM val = ReaderT (Env val) Err

-- | Operations on environment
lookupVar :: Var -> Env val -> Err (StaticVal val)
lookupVar var env@(Env oEnv lEnv) = case Map.lookup var lEnv of
  Just exp -> return (exp, env)
  Nothing -> case Map.lookup var oEnv of
    Just sVal -> return sVal
    Nothing -> fail $ "TypeCheckError: Undeclared variable " ++ show var

-- | Insert static expression directly to environment
assignStaticVal :: Var -> StaticVal val -> Env val -> Env val
assignStaticVal v sVal (Env oEnv lEnv) = Env (Map.insert v sVal oEnv) lEnv

-- | Extract variable from declaration
-- TODO: maybe extract many vars, if declaration is "Pattern = Exp"
getVar :: Decl -> Var
getVar (Signature (Sign var ty)) = var
getVar (FunDecl var _ _) = var
getVar (TmpVarDecl var _) = var

splitDecl :: Decl -> (Var, val)
splitDecl = undefined
-- TODO: equivalent of getVal and getVar



----------------------- Declarations evaluation ------------------------
-- | Set value of a variable in local environment.
-- Each variable may be present only once in a local environment.
setLocalVar :: Var -> val -> LocalEnv val -> Err (LocalEnv val)
setLocalVar var exp localEnv = if Map.member var localEnv
  then Bad $ "TypeCheckError: Variable " ++ show var ++ " defined twice"
  else Ok $ Map.insert var exp localEnv

-- | Turn list of declarations to a map, where each varibale has its own
-- expression as value
-- TODO: we assume, that each variable has one expression bound
-- TODO: implements patterns
collectLocalDecl :: EnvVal val => [Decl] -> Err (LocalEnv val)
collectLocalDecl decls = foldM insertDecl Map.empty decls
  where insertDecl lEnv d = case getVal d of {
    Just val -> setLocalVar (getVar d) val lEnv;
    Nothing -> Ok lEnv
  }
-- | Insert local environment to outer environment.
localToOuterEnv :: LocalEnv val -> OuterEnv val -> OuterEnv val
localToOuterEnv localEnv outerEnv = Map.foldrWithKey (insertVar (Env outerEnv localEnv)) outerEnv localEnv
  where
    insertVar :: Env val -> Var -> val -> OuterEnv val -> OuterEnv val
    insertVar env var exp outerEnv = Map.insert var (exp, env) outerEnv

-- | Add local environment to existing environment by merging
-- old local environment with old outer environment and by setting
-- the given local environment as a new one
expandEnv :: LocalEnv val -> Env val -> Env val
expandEnv lEnv (Env oldOEnv oldLEnv) = Env (localToOuterEnv oldLEnv oldOEnv) lEnv

-- | Turn list of declaration to local environment and expand given environment
evalDecls :: EnvVal val => [Decl] -> Env val -> Err (Env val)
evalDecls localDecls env = do
  localEnv <- collectLocalDecl localDecls
  return $ expandEnv localEnv env
