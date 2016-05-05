module Types where

import qualified Data.Map as Map

import AbsMbCore
import ErrM

-- | Data environment
type DataEnv = Map.Map TyCon ([TyVar], Map.Map Con [Type])

-- | Build data environment basing on 'data' declarations
buildDataEnv :: [TopDecl] -> Err DataEnv
buildDataEnv [] = Ok Map.empty
buildDataEnv x = Bad $ show x

----------------- Static type check ---------------------------
checkTypes :: [TopDecl] -> Err String
checkTypes _ = Ok "Types are OK"
