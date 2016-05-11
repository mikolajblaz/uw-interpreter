

module AbsMbCore where

-- Haskell module generated by the BNF converter




newtype Var = Var String deriving (Eq, Ord, Show, Read)
newtype Con = Con String deriving (Eq, Ord, Show, Read)
data Body = Body [TopDecl]
  deriving (Eq, Ord, Show, Read)

data TopDecl = DataDecl DataDecl | Decl Decl
  deriving (Eq, Ord, Show, Read)

data Decl = Signature Signature | VarDecl Var Exp
  deriving (Eq, Ord, Show, Read)

data Signature = Sign Var Type
  deriving (Eq, Ord, Show, Read)

data DataDecl = Data Con [Constr]
  deriving (Eq, Ord, Show, Read)

data Constr = DataCon Con [Type]
  deriving (Eq, Ord, Show, Read)

data Type
    = FunType Type Type
    | TyCon Con
    | TupleType Type [Type]
    | ListType Type
  deriving (Eq, Ord, Show, Read)

data Alt = Alt Pat Exp
  deriving (Eq, Ord, Show, Read)

data Exp
    = Lambda [Signature] Exp
    | Let [Decl] Exp
    | If Exp Exp Exp
    | Case Exp [Alt]
    | FApp Exp Exp
    | VarExp Var
    | ConExp Con
    | LitExp Literal
    | TupleExp Exp [Exp]
    | ListExp [Exp]
    | OOr Exp Exp
    | OAnd Exp Exp
    | EOpE Exp CompOp Exp
    | OAdd Exp Exp
    | OSub Exp Exp
    | ONeg Exp
    | OMul Exp Exp
    | ODiv Exp Exp
  deriving (Eq, Ord, Show, Read)

data CompOp = OEq | ONeq | OLt | OLte | OGt | OGte
  deriving (Eq, Ord, Show, Read)

data Pat
    = ConPat Con [Pat]
    | VarPat Var
    | LitPat Literal
    | WildCard
    | TuplePat Pat [Pat]
    | ListPat [Pat]
  deriving (Eq, Ord, Show, Read)

data Literal
    = IntLit Integer
    | DoubleLit Double
    | CharLit Char
    | StringLit String
  deriving (Eq, Ord, Show, Read)

