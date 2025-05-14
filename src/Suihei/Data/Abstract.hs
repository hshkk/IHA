module Suihei.Data.Abstract (
    Program, 
    Statement  (..),
    Environment, Binding, 
    Expression (..), 
    Value      (..), 
    Variable,
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
                | Expression :>. Expression
                | Expression :^. Expression
                | Expression :>| Expression
                | Expression :|< Expression
                | Infer Expression

data Value = Progression Progression
           | Music Music

data Effect = Out  Expression
            | Outf Key Expression
            | Midi Expression