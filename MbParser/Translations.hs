module Translations where

import MbAbsCore

data VarLambda = VarLambda [Var] Exp

simplifyLambda :: Exp -> VarLamda
simplifyLambda (Lambda [pats] exp) = undefined
