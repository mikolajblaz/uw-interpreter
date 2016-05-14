module Environment where

import Control.Monad ( foldM )
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

import AbsMbCore
import ErrM
import PrintMbCore

-- | A value that can be kept in an environment
-- Instances: Exp and Type
class EnvVal val where
  -- | Extract value from declaration
  -- 'Nothing' is not an error, it means that value cannot be extracted from
  -- specific type of declarations
  getVal :: Decl -> Maybe val

-- | Whole environment
data Env val = Env (OuterEnv val) (LocalEnv val) deriving (Show)
-- | Local environment includes variables introduced by the last 'let'
type LocalEnv val = Map.Map Var val
-- | Outer environment includes other variables
type OuterEnv val = Map.Map Var (StaticVal val)
-- | Value with bound environment
type StaticVal val = (val, Env val)


-- | A monad to evaluate program in
type EvalM val = ReaderT (Env val) Err

-- | Create empty environment
emptyEnv :: Env val
emptyEnv = Env Map.empty Map.empty

----------------------- Operations on environment ----------------------
lookupVar :: Var -> Env val -> Err (StaticVal val)
lookupVar var env@(Env oEnv lEnv) = case Map.lookup var lEnv of
  Just exp -> return (exp, env)
  Nothing -> case Map.lookup var oEnv of
    Just sVal -> return sVal
    Nothing -> fail $ "TypeCheckError: Undeclared variable " ++ show var

-- | Insert static expression directly to environment
assignStaticVal :: Var -> StaticVal val -> Env val -> Env val
assignStaticVal v sVal (Env oEnv lEnv) = let tmpOEnv = (localToOuterEnv lEnv oEnv) in
  Env (Map.insert v sVal tmpOEnv) Map.empty


----------------------- Declarations evaluation ------------------------
-- | Extract variable from declaration
getVar :: Decl -> Var
getVar (Signature (Sign var ty)) = var
getVar (VarDecl var _) = var

-- | Set value of a variable in local environment.
-- Each variable may be present only once in a local environment.
setLocalVar :: Var -> val -> LocalEnv val -> Err (LocalEnv val)
setLocalVar var exp localEnv = if Map.member var localEnv
  then Bad $ "TypeCheckError: Variable " ++ show var ++ " defined twice"
  else Ok $ Map.insert var exp localEnv

setOuterVar :: Var -> StaticVal val -> OuterEnv val -> Err (OuterEnv val)
setOuterVar var sVal outerEnv = if Map.member var outerEnv
  then Bad $ "TypeCheckError: Variable " ++ show var ++ " defined twice"
  else Ok $ Map.insert var sVal outerEnv

-- | Turn list of declarations to a map, where each varibale has its own
-- expression as value
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
expandEnv lEnv (Env oldOEnv oldLEnv) = Env (localToOuterEnv lEnv (localToOuterEnv oldLEnv oldOEnv)) Map.empty

-- | Add outer environment to existing environment
expandOuterEnv :: OuterEnv val -> Env val -> Env val
expandOuterEnv oEnv (Env oldOEnv oldLEnv) = Env (Map.union oEnv (localToOuterEnv oldLEnv oldOEnv)) Map.empty

-- | Turn list of declaration to local environment and expand given environment
evalDecls :: EnvVal val => [Decl] -> Env val -> Err (Env val)
evalDecls localDecls env = do
  localEnv <- collectLocalDecl localDecls
  return $ expandEnv localEnv env




--------------------------- Data environment ---------------------------

type DataEnv = Map.Map Con (Con, [Type])

-- | Build data environment basing on 'data' declarations
buildDataEnv :: [TopDecl] -> Err DataEnv
buildDataEnv ds = do
  foldM (flip buildData) Map.empty ds

buildData :: TopDecl -> DataEnv -> Err DataEnv
buildData (DataDecl (Data con constrs)) env = foldM (insertCon con) env constrs
  where
    insertCon :: Con -> DataEnv -> Constr -> Err DataEnv
    insertCon dataCon env (DataCon con ts) = if Map.member con env
      then fail $ "TypeCheckError: Type constructor " ++ show con ++ " already declared"
      else return $ Map.insert con (dataCon, ts) env


getConData :: Con -> DataEnv -> Err (Con, [Type])
getConData con env = case Map.lookup con env of
  Nothing -> fail $ "TypeCheckError: Undeclared constructor " ++ show con
  Just dat -> return $ dat

getConTypes :: Con -> DataEnv -> Err [Type]
getConTypes con env = getConData con env >>= return . snd

-- | Get type of type constructor
conToType :: Con -> DataEnv -> Err Type
conToType con env = do
  (dataCon, ts) <- getConData con env
  return $ generateConType con ts dataCon

-- | Get type of type constructor based on list of type arguments and target
-- data type
generateConType :: Con -> [Type] -> Con -> Type
generateConType con [] dataCon = TyCon dataCon
generateConType con (t:ts) dataCon = FunType t $ generateConType con ts dataCon
