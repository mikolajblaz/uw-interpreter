module Types where

import Control.Monad.Reader
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment

-- | Data environment
type DataEnv = Map.Map TyCon ([TyVar], Map.Map Con [Type])

-- | Build data environment basing on 'data' declarations
buildDataEnv :: [TopDecl] -> Err DataEnv
buildDataEnv [] = Ok Map.empty
buildDataEnv x = Bad $ show x

----------------- Static type check ---------------------------
type StaticType = StaticVal Type
type TypeM = EvalM Type

instance EnvVal Type where
  getVal (Signature (Sign var ty)) = ty
  getVal (FunDecl _ _ _) = undefined
  getVal (TmpVarDecl _ _) = undefined

staticTypeCheck :: Exp -> Env Type -> Err Type
staticTypeCheck exp env = runReaderT (checkType exp) env

compareType :: Exp -> Type -> TypeM Type
compareType e t = do
  t' <- checkType e
  if t == t'
    then return t
    else fail $ "Expected " ++ show t ++ ", got " ++ show t'

compareTypes :: [Exp] -> [Type] -> TypeM ()
compareTypes es ts = mapM_ (uncurry compareType) $ zip es ts

checkType :: Exp -> TypeM Type
checkType _ = return $ GTyCon UnitTyCon
