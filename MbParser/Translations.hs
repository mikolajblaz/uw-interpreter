module Translations where

import AbsMbCore

data VarLambda = VarLambda [Var] Exp

simplifyLambda :: Exp -> VarLambda
simplifyLambda (Lambda [pats] exp) = undefined
