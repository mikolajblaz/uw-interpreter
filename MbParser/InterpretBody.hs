module InterpretBody where

import qualified Data.List ( partition )
import qualified Data.Map as Map

import AbsMbCore
import ErrM

import Environment
import Expressions
import StdLib
import Translations
import Types


-- | Main interpreting function
interpretBody :: Body -> Err String
interpretBody (Body topdecls) = do
    dataEnv <- buildDataEnv dataDecls
    interpretMain decls dataEnv
  where
    (dataDecls, decls) = Data.List.partition isDataDecl $ stdLibDecls ++ topdecls
    isDataDecl (DataDecl _) = True
    isDataDecl _  = False


----------------------- Declarations -----------------------
chooseVarDecls :: [TopDecl] -> [Decl]
chooseVarDecls ((Decl d):ds) = (d : chooseVarDecls ds)
chooseVarDecls [] = []

interpretMain :: [TopDecl] -> DataEnv -> Err String
interpretMain topDecls dataEnv = let
    decls = chooseVarDecls topDecls
    rootExp = Let decls (VarExp (Var "main"))
  in do
    finalType <- staticTypeCheck rootExp emptyEnv dataEnv
    finalVal <- runExp rootExp emptyEnv
    return $ show finalVal ++ ", final type: " ++ show finalType
