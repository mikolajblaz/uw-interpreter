module Environment where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

import AbsMbCore
import ErrM

-- TODO
--type DataEnv = Map Con DataDecl
type DataEnv = String

-- | Bindings, i.e. map of variables in scope
newtype Bindings = Bindings (Map.Map Var VarEnv)
-- | Variable declarations
type VarEnv = Map.Map Var [Decl]

-- | A monad to evaluate program in
type M = ReaderT (VarEnv, Bindings) Err



lookupVar name var_map = e
  where ([TmpVarDecl var e], Just bindings) = fromJust $ Map.lookup name var_map

setValue var val var_map = Map.insert var val var_map
setStateValue var val = modify (setValue var val)


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

-- | Insert declarations (of 1 variable) from local scope to bindings
-- from outer scope.
-- Simply replaces the old value.
insertDeclToBinding :: Var -> [Decl] -> Bindings -> Bindings
insertDeclToBinding var decls = Map.insert var (decls, Nothing)

-- | Insert all declarations from local scope to bindings from outer scope.
insertLocalToOuter :: [Decl] -> Bindings -> Bindings
insertLocalToOuter localDecl bindings =
  Map.foldrWithKey insertDeclToBinding bindings $ collectLocalDecl localDecl
