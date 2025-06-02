{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Suihei.Control.Interpreter (
    interpret
) where

import Suihei.Data.Abstract (
    Statement  (..),
    Environment,
    Expression (..),
    Value      (..),
    Effect     (..))

import Suihei.Control.Compiler (cout)
import Suihei.Control.Prelude (Trans (trans))
import Suihei.Control.Reification (reify)
import Suihei.Data.Chordal (Analysis (Shift), IAnalysis (Ia))
import Suihei.Data.Notal (INote(..))
import Suihei.Data.Composition (
    fore, aft,
    Total (duration), Take (takenFrom, droppedFrom),
    pattern Unit, pattern (:>), pattern (:^), trim, Music, Progression)

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (
    StateT,
    modify, get, execStateT)
import Data.Maybe (fromJust)
import Suihei.Control.Inference (infer)

import Prelude hiding (exp)

type State a = StateT Environment IO a

interpret :: [Statement] -> IO ()
interpret sts = execStateT (mapM_ statement sts) [] >> mempty

statement :: Statement -> State ()
statement (Def var exp) = do
    env <- get
    let val = expression exp env
    modify ((var, val) :)
statement (Eff eff) = do
    env <- get
    lift (effect eff env)

effect :: Effect -> Environment -> IO ()
-- If no key is provided, then the associated value is printed in relative notation.
effect (Out exp Nothing) env =
    let val = expression exp env in
    case val of
        Music m       -> print m
        Progression p -> print p
effect (Out exp (Just key)) env =
    let val = expression exp env in
    case val of
        -- Converts Music/Progression values so that they can be run through a different pretty-printer.
        -- In a future iteration, this system should be rewritten.
        Music m       -> print (In <$> trans m key)
        Progression p -> print (Ia <$> trans p key)
effect (Lily exp key file) env =
    let val = expression exp env in
    cout val key file
effect (Anno exp key file) env =
    let val = expression exp env
        inf = expression (Infer exp key) env in
        case (inf, val) of
            (Progression p, Music m) -> cout (p, m) key file
            _                        -> error "Control.Interpreter: Unexpected annotation result"

expression :: Expression -> Environment -> Value

expression (Value val) _ = val

expression (Variable var) env
    | Just val <- lookup var env = val
    | otherwise = error "Control.Interpreter: Undefined variable"

expression (PartialShift key exp) env
    | Progression (Unit (ana, dur)) <- expression exp env = Progression . Unit $ (Shift key ana, dur)
    | Progression p@(_ :> _) <- expression exp env =
        let Unit (ana, dur) = fore p in
        Progression (Unit (Shift key ana, dur) :> dur `takenFrom` p)
    | otherwise = error "Control.Interpreter: Invalid shift"

expression (exp1 :>. exp2) env
    = let m1' = extractM_ (expression exp1 env)
          m2' = extractM_ (expression exp2 env) 
          p1' = extractP_ (expression exp1 env)
          p2' = extractP_ (expression exp2 env) in
      case (m1', m2', p1', p2') of
          (Just m1, Just m2, _, _) -> Music (m1 :> m2)
          (_, _, Just p1, Just p2) -> Progression (p1 :> p2)
          _ -> error "Control.Interpret: Invalid serial composition"

expression (exp1 :^. exp2) env
    = let m1' = extractM_ (expression exp1 env)
          m2' = extractM_ (expression exp2 env) in
      case (m1', m2') of
          (Just m1, Just m2) -> Music (m1 :^ m2)
          _ -> error "Control.Interpret: Invalid parallel composition"

expression (exp1 :>| exp2) env
    = let m1' = extractM_ (expression exp1 env)
          m2' = extractM_ (expression exp2 env) 
          p1' = extractP_ (expression exp1 env)
          p2' = extractP_ (expression exp2 env) in
      case (m1', m2', p1', p2') of
          (Just m1, Just m2, _, _) ->
              let m3 = fore m2
                  dur = duration m3 in
              Music . fromJust . trim $ (m1 :> dur `droppedFrom` m2)
          (_, _, Just p1, Just p2) -> 
              let p3 = fore p2
                  dur = duration p3 in
              Progression . fromJust . trim $ (p1 :> dur `droppedFrom` p2)
          _ -> error "Control.Interpret: Invalid serial composition"

expression (exp1 :|< exp2) env
    = let m1' = extractM_ (expression exp1 env)
          m2' = extractM_ (expression exp2 env) 
          p1' = extractP_ (expression exp1 env)
          p2' = extractP_ (expression exp2 env) in
      case (m1', m2', p1', p2') of
          (Just m1, Just m2, _, _) ->
              let m3 = aft m1
                  dur = duration m1 - duration m3 in
              Music . fromJust . trim $ (dur `takenFrom` m1 :> m2)
          (_, _, Just p1, Just p2) ->
              let p3 = aft p1
                  dur = duration p1 - duration p3 in
              Progression . fromJust . trim $ (dur `takenFrom` p1 :> p2)
          _ -> error "Control.Interpreter: Invalid serial composition"

-- When inferring with aspect to a key, the corresponding (music) expression should be shifted by that key.
expression (Infer exp key) env
    = let m' = extractM_ (expression exp env)
          p' = extractP_ (expression exp env) in
      case (m', p') of
          (Just m, _) -> Progression (infer m key)
          (_, Just p) -> Progression (trans p key)
          _ -> error "Control.Interpreter: Invalid inference"

expression (Reify exp key) env
    = let m' = extractM_ (expression exp env)
          p' = extractP_ (expression exp env) in
      case (m', p') of
          (Just m, _) -> Music (trans m key)
          (_, Just p) -> Music (trans (reify p) key)
          _ -> error "Control.Interpreter: Invalid reification"

extractP_ :: Value -> Maybe Progression
extractP_ (Progression p) = Just p
extractP_ _ = Nothing

extractM_ :: Value -> Maybe Music
extractM_ (Music m) = Just m
extractM_ _ = Nothing