module StdLib where

import AbsMbCore
import ErrM

import Environment

dataBool = DataDecl (Data (Con "Bool") [DataCon (Con "True") [], DataCon (Con "False") []])

dataList = DataDecl (Data (Con "List") [DataCon (Con "Nil") [], DataCon (Con "Cons") [TyCon $ Con "Int", TyCon $ Con "List"]])
dataIntMaybe = DataDecl (Data (Con "IntMaybe") [DataCon (Con "Nothing") [], DataCon (Con "Just") [TyCon $ Con "Int"]])
dataIntEither = DataDecl (Data (Con "IntEither") [DataCon (Con "Left") [TyCon $ Con "Int"], DataCon (Con "Right") [TyCon $ Con "Int"]])

listFunDecls = zipWith (\x y -> Decl (Signature $ Sign (Var x) $ FunType (TyCon $ Con "List") $ TyCon $ Con y))
               ["empty", "head", "tail"] ["Bool", "Int", "List"]
-- funEmptyDecl = Decl (Signature $ Sign (Var "empty") $ FunType (TyCon $ Con "List") $ TyCon $ Con "Bool")
-- funHeadDecl = Decl (Signature $ Sign (Var "head") $ FunType (TyCon $ Con "List") $ TyCon $ Con "Int")
-- funTailDecl = Decl (Signature $ Sign (Var "tail") $ FunType (TyCon $ Con "List") $ TyCon $ Con "Int")

funEmpty = Decl (VarDecl (Var "empty") $ Lambda [Sign (Var "x") (TyCon $ Con "List")] $ Case (VarExp $ Var "x") [
             Alt (ConPat (Con "Nil") []) $ ConExp $ Con "True",
             Alt WildCard $ ConExp $ Con "False"
           ])
funHead = Decl (VarDecl (Var "head") $ Lambda [Sign (Var "x") (TyCon $ Con "List")] $ Case (VarExp $ Var "x")
            ([Alt (ConPat (Con "Cons") [VarPat $ Var "h", WildCard]) $ VarExp $ Var "h"]))
funTail = Decl (VarDecl (Var "tail") $ Lambda [Sign (Var "x") (TyCon $ Con "List")] $ Case (VarExp $ Var "x") [
            Alt (ConPat (Con "Nil") []) $ ConExp $ Con "Nil",
            Alt (ConPat (Con "Cons") [WildCard, VarPat $ Var "t"]) $ VarExp $ Var "t"
          ])


stdLibDecls :: [TopDecl]
stdLibDecls = [dataBool, dataList, dataIntMaybe, dataIntEither, funEmpty, funHead, funTail] ++ listFunDecls
