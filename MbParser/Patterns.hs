module Patterns where

import AbsMbCore
import ErrM

import Environment
import Expressions

-- data Exp
--     | VarExp Var
--     | GConExp GCon
--     | LitExp Literal
--     | TupleExp Exp [Exp]
--     | ListExp [Exp]
--   deriving (Eq, Ord, Show, Read)
--
-- data Pat
--     = ManyGConPat GCon [Pat]
--     | VarPat Var
--     | ZeroGConPat GCon
--     | LitPat Literal
--     | WildCard
--     | TuplePat Pat [Pat]
--     | ListPat [Pat]
--   deriving (Eq, Ord, Show, Read)
--
-- data GCon = SimpleCon Con | UnitCon | ListCon | TupleCon [Comma]
--   deriving (Eq, Ord, Show, Read)

matchAgainst :: Pat -> Exp -> Env -> LocalEnv -> Err LocalEnv
matchAgainst pat exp env = do
  evaledExp <- runExp exp env
  case (pat, evaledExp) of
    (VarPat var, e) -> setLocalVar var e
    (WildCard, e) -> Ok
