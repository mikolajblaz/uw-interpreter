module InterpretBody where

import AbsMbCore

import ErrM

interpretBody :: a -> Err String
interpretBody _ = Bad "No interpreter found"
