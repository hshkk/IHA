module Suihei.Text.Ancillary.Combinator where

import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec (
    GenParser, 
    try, (<|>))

import Data.Functor.Identity (Identity)

(?<|>) :: GenParser tok u a -> ParsecT [tok] u Identity a -> ParsecT [tok] u Identity a
p1 ?<|> p2 = try p1 <|> p2
infixr 1 ?<|>

returnM :: Monad m => a -> m (Maybe a)
returnM a = return (Just a)