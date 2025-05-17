{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Interpreter (
    state
) where

import Suihei.Data.Abstract (
    Statement  (..),
    Environment,
    Expression (..),
    Value      (..),
    Effect     (..))

import Suihei.Data.Chordal (Analysis (Shift))
import Suihei.Data.Composition (
    fore, aft,
    Total (duration), Take (takenFrom, droppedFrom),
    pattern Unit, pattern (:>), pattern (:^), trim)

import Control.Monad.Trans.State (StateT, modify, get)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Maybe (fromJust)

type SuiheiState a = StateT Environment IO a

state :: Statement -> SuiheiState ()
state (Def var exp) = do
    env <- get
    let val = interpret exp env
    modify ((var, val) :)
state (Eff eff) = do
    env <- get
    lift (effect eff env)

effect :: Effect -> Environment -> IO ()
effect (Out exp) env =
    let val = interpret exp env in
    print val
effect _ _ = undefined

interpret :: Expression -> Environment -> Value

interpret (Value val) _ = val

interpret (Variable var) env
    | Just val <- lookup var env = val
    | otherwise = error "Control.Interpreter: Undefined variable"

interpret (PartialShift key exp) env
    | Progression (Unit (ana, dur)) <- interpret exp env = Progression . Unit $ (Shift key ana, dur)
    | Progression p@(_ :> _) <- interpret exp env =
        let Unit (ana, dur) = fore p in
        Progression (Unit (Shift key ana, dur) :> dur `takenFrom` p)
    | otherwise = error "Control.Interpreter: Invalid shift"

interpret (exp1 :>. exp2) env
    | Progression p1 <- interpret exp1 env
    , Progression p2 <- interpret exp2 env = Progression (p1 :> p2)
    | Music m1 <- interpret exp1 env
    , Music m2 <- interpret exp2 env = Music (m1 :> m2)
    | otherwise = error "Control.Interpreter: Invalid serial composition"

interpret (exp1 :^. exp2) env
    | Music m1 <- interpret exp1 env
    , Music m2 <- interpret exp2 env = Music (m1 :^ m2)
    | otherwise = error "Control.Interpreter: Invalid parallel composition"

interpret (exp1 :>| exp2) env
    | Progression p1 <- interpret exp1 env
    , Progression p2 <- interpret exp2 env =
        let p3 = fore p2
            dur = duration p3 in
        Progression (p1 :> dur `droppedFrom` p2)
    | Music m1 <- interpret exp1 env
    , Music m2 <- interpret exp2 env =
        let m3 = fore m2
            dur = duration m3 in
        Music . fromJust . trim $ (m1 :> dur `droppedFrom` m2)
    | otherwise = error "Control.Interpreter: Invalid serial composition"

interpret (exp1 :|< exp2) env
    | Progression p1 <- interpret exp1 env
    , Progression p2 <- interpret exp2 env =
        let p3 = aft p1
            dur = duration p1 - duration p3 in
        Progression (dur `takenFrom` p1 :> p2)
    | Music m1 <- interpret exp1 env
    , Music m2 <- interpret exp2 env =
        let m3 = aft m1
            dur = duration m1 - duration m3 in
        Music . fromJust . trim $ (dur `takenFrom` m1 :> m2)
    | otherwise = error "Control.Interpreter: Invalid serial composition"

interpret (Infer exp) env = undefined