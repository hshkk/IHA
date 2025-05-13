module Suihei.Data.Abstract (
    Env, Bind, 
    Exp (..), 
    Val (..), 
    Var
) where

import Suihei.Data.Composition (Progression, Music)

type Var = String

type Env = [Bind]
type Bind = (Var, Val)

data Exp = Val Val
         | Var Var
         | Exp :>. Exp
         | Exp :^. Exp
         | Exp :>| Exp
         | Exp :|< Exp
         | Infer Exp

data Val = Progression Progression
         | Music Music