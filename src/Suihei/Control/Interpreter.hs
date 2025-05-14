module Suihei.Control.Interpreter where

import Suihei.Data.Abstract (
    Program, 
    Statement  (..),
    Environment, Binding, 
    Expression (..), 
    Value      (..), 
    Variable,
    Effect     (..))

interpret :: Expression -> Environment -> Value
interpret exp env = undefined