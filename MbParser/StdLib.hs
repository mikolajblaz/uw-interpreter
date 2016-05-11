module StdLib where

import AbsMbCore
import ErrM

import Environment

dataBool = DataDecl (Data (Con "Bool") [DataCon (Con "True") [], DataCon (Con "False") []])

stdLibDecls :: [TopDecl]
stdLibDecls = [dataBool]
