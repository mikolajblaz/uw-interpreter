module InterpretBody where

import qualified Data.List ( partition )
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment
import Expressions
import Translations

failure :: Show a => a -> Err String
failure x = Bad $ "Undefined case: " ++ show x

-- | Main interpreting function
interpretBody :: Body -> Err String
interpretBody (Body topdecls) = do
    -- dataEnv <- buildDataEnv dataDecls
    interpretTopDecls decls -- dataEnv
  where
    (dataDecls, decls) = Data.List.partition isDataDecl topdecls
    isDataDecl (DataDecl _) = True
    isDataDecl _  = False


----------------------- Types -----------------------

-- buildDataEnv :: [TopDecl] -> Err DataEnv
-- buildDataEnv [] = Ok "Empty DataEnv"
-- buildDataEnv x = failure x

----------------------- Declarations -----------------------

interpretTopDecls :: [TopDecl] -> Err String
interpretTopDecls decls = interpretTmpVarDecls $ chooseVarDecls decls
  where
    chooseVarDecls ((Decl d):ds) = (d : chooseVarDecls ds)
    chooseVarDecls [] = []

interpretTmpVarDecls :: [Decl] -> Err String
interpretTmpVarDecls decls = let
    rootExp = Let decls (VarExp (Var "main"))
    initEnv = Env Map.empty Map.empty
  in do
    finalVal <- runExp rootExp initEnv
    return $ show finalVal
