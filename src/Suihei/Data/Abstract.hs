module Suihei.Data.Abstract (
    Program, 
    Statement  (..),
    Environment, Binding, 
    Expression (..), 
    Value      (..), 
    Variable,
    File,
    Effect     (..)
) where

import Suihei.Data.Chordal (Key)
import Suihei.Data.Composition (Progression, Music)

type Program = [Statement]

data Statement = Def Variable Expression
               | Eff Effect

type Variable = String

type Environment = [Binding]
type Binding = (Variable, Value)

data Expression = Value Value
                | Variable Variable
                | PartialShift Key Expression
                | Expression :>. Expression
                | Expression :^. Expression
                | Expression :>| Expression
                | Expression :|< Expression
                | Infer Expression

infixl 5 :>.
infixl 6 :^.
infixl 5 :>|
infixl 5 :|<

data Value = Progression Progression
           | Music Music

type File = Maybe String

data Effect = Out  Expression Key
            | Lily Expression Key File
            | Anno Expression Key File
