module IHA.Data.Abstract (
    Program, 
    Statement  (..),
    Environment, Binding, 
    Expression (..), 
    Value      (..), 
    Variable,
    File,
    Effect     (..)
) where

import IHA.Data.Chordal (Key)
import IHA.Data.Composition (Progression, Music)

import IHA.Text.Prettyprinter ()

type Program = [Statement]

data Statement = Def Variable Expression
               | Eff Effect
               deriving Show

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
                | Infer Expression Key
                | Reify Expression Key
                deriving Show

infixl 5 :>.
infixl 6 :^.
infixl 5 :>|
infixl 5 :|<

data Value = Progression Progression
           | Music Music
           deriving Show

type File = Maybe String

data Effect = Out  Expression (Maybe Key)
            | Lily Expression Key File
            | Anno Expression Key File
            deriving Show
