module Environment where

import Control.Monad ( foldM )
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

import AbsMbCore
import ErrM

class EnvVal a where
  getVal :: Decl -> a

-- | Whole environment
data Env val = Env OuterEnv LocalEnv
-- | Local environment includes variables introduced by the last 'let'
type LocalEnv val = Map.Map Var val
-- | Outer environment includes other variables
type OuterEnv val = Map.Map Var (StaticVal val)
-- | Expression which knows how to be executed (in which environment)
type StaticVal val = (val, Env)


-- | A monad to evaluate program in
type EvalM = ReaderT Env Err

-- | Operations on environment
lookupVar :: Var -> Env -> Err StaticExp
lookupVar var env@(Env oEnv lEnv) = case Map.lookup var lEnv of
  Just exp -> Ok (exp, env)
  Nothing -> case Map.lookup var oEnv of
    Just sExp -> Ok sExp
    Nothing -> Bad $ "RuntimeError: Undefined variable " ++ show var

-- | Insert static expression directly to environment
assignStaticExp :: Var -> StaticExp -> Env -> Env
assignStaticExp v sExp (Env oEnv lEnv) = Env (Map.insert v sExp oEnv) lEnv

-- | Extract variable from declaration
-- TODO: maybe extract many vars, if declaration is "Pattern = Exp"
getVar :: Decl -> Var
getVar (Signature var ty) = undefined
getVar (FunDecl var _ _) = var
getVar (TmpVarDecl var _) = var

-- | Extract expression from declaration
getExp :: Decl -> Exp
getExp (Signature var ty) = undefined
getExp (FunDecl _ _ _) = undefined
getExp (TmpVarDecl _ exp) = exp

splitDecl :: Decl -> (Var, Exp)
splitDecl = undefined
-- TODO: equivalent of above getExp and getVar



----------------------- Declarations evaluation ------------------------
-- | Set value of a variable in local environment.
-- Each variable may be present only once in a local environment.
setLocalVar :: Var -> Exp -> LocalEnv -> Err LocalEnv
setLocalVar var exp localEnv = if Map.member var localEnv
  then Bad $ "RuntimeError: Variable " ++ show var ++ " defined twice"
  else Ok $ Map.insert var exp localEnv

-- | Turn list of declarations to a map, where each varibale has its own
-- expression as value
-- TODO: we assume, that each variable has one expression bound
-- TODO: implements patterns
collectLocalDecl :: [Decl] -> Err LocalEnv
collectLocalDecl decls = foldM insertDecl Map.empty decls
  where insertDecl lEnv d = setLocalVar (getVar d) (getExp d) lEnv

-- | Insert local environment to outer environment.
localToOuterEnv :: LocalEnv -> OuterEnv -> OuterEnv
localToOuterEnv localEnv outerEnv = Map.foldrWithKey (insertVar (Env outerEnv localEnv)) outerEnv localEnv
  where
    insertVar :: Env -> Var -> Exp -> OuterEnv -> OuterEnv
    insertVar env var exp outerEnv = Map.insert var (exp, env) outerEnv

-- | Add local environment to existing environment by merging
-- old local environment with old outer environment and by setting
-- the given local environment as a new one
expandEnv :: LocalEnv -> Env -> Env
expandEnv lEnv (Env oldOEnv oldLEnv) = Env (localToOuterEnv oldLEnv oldOEnv) lEnv

-- | Turn list of declaration to local environment and expand given environment
evalDecls :: [Decl] -> Env -> Err Env
evalDecls localDecls env = do
  localEnv <- collectLocalDecl localDecls
  return $ expandEnv localEnv env
