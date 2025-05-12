module Suihei.Control.Interpreter where

import Suihei.Data.Abstract (
    Env, Bind, 
    Exp (..), 
    Val (..), 
    Var)

interpret :: Exp -> Env -> Val
interpret exp env = undefined