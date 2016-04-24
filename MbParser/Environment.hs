module Environment where

import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

import AbsMbCore
import ErrM

-- TODO
--type DataEnv = Map Con DataDecl
type DataEnv = String

-- | Whole environment
data Env = Env OuterEnv LocalEnv
-- | Local environment includes variables introduced by the last 'let'
type LocalEnv = Map.Map Var Exp
-- | Outer environment includes other variables
type OuterEnv = Map.Map Var StaticExp
-- | Expression which knows how to be executed (in which environment)
type StaticExp = (Exp, Env)


-- | A monad to evaluate program in
type EvalM = ReaderT Env Err

-- | Operations on environment
lookupVar :: Var -> Env -> Maybe StaticExp
lookupVar var env@(Env oEnv lEnv) = case Map.lookup var lEnv of
  Just exp -> Just (exp, env)
  Nothing -> case Map.lookup var oEnv of
    Just sExp -> Just sExp
    Nothing -> Nothing

setLocalVar :: Var -> Exp -> LocalEnv -> LocalEnv
setLocalVar var exp localEnv = Map.insert var exp localEnv
-- TODO: check if variable is already set

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


-- | Turn list of declarations to a map, where each varibale has its own
-- expression as value
-- TODO: we assume, that each variable has one expression bound
-- TODO: implements patterns
collectLocalDecl :: [Decl] -> LocalEnv
collectLocalDecl decls = foldr insertDecl Map.empty decls
  where insertDecl d = setLocalVar (getVar d) (getExp d)

-- | Insert all declarations from local scope to bindings from outer scope.
localToOuterEnv :: LocalEnv -> OuterEnv -> OuterEnv
localToOuterEnv localEnv outerEnv = Map.foldrWithKey (insertVar (Env outerEnv localEnv)) outerEnv localEnv
  where
    insertVar :: Env -> Var -> Exp -> OuterEnv -> OuterEnv
    insertVar env var exp outerEnv = Map.insert var (exp, env) outerEnv

-- | Add declarations to exisitng environment
evalDecls :: [Decl] -> Env -> Env
evalDecls localDecls (Env oldOuterEnv oldLocalEnv) = Env outerEnv localEnv
  where
    outerEnv = localToOuterEnv oldLocalEnv oldOuterEnv
    localEnv = collectLocalDecl localDecls
