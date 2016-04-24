module Environment where

import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

import AbsMbCore
import ErrM

-- TODO
--type DataEnv = Map Con DataDecl
type DataEnv = String

newtype Env = Env (Map.Map Var StaticExp)
type StaticExp = (Exp, Env, LocalEnv)
type LocalEnv = Map.Map Var Exp -- TODO: maybe (Exp, Env)

-- | A monad to evaluate program in
type M = ReaderT Env Err

-- | Operations on environment
lookupLocalVar :: Var -> LocalEnv -> Exp
lookupLocalVar var localEnv = fromJust $ Map.lookup var localEnv
lookupVar :: Var -> Env -> StaticExp
lookupVar var (Env env) = fromJust $ Map.lookup var env

setVar :: Var -> Exp -> LocalEnv -> LocalEnv
setVar var exp localEnv = Map.insert var exp localEnv
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
  where insertDecl d = Map.insert (getVar d) (getExp d)

-- | Insert all declarations from local scope to bindings from outer scope.
localToOuterEnv :: [Decl] -> Env -> Env
localToOuterEnv localDecls outerEnv = Map.foldrWithKey (insertVar outerEnv localEnv) outerEnv localEnv
  where
    localEnv = collectLocalDecl localDecls
    insertVar :: Env -> LocalEnv -> Var -> Exp -> Env -> Env
    insertVar outerEnv localEnv var exp (Env env) = Env $ Map.insert var (exp, outerEnv, localEnv) env
