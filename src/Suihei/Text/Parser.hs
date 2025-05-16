module Suihei.Text.Parser (
    parseLn
) where

import Suihei.Data.Abstract (
    Statement  (..))
import Suihei.Text.Ancillary.State (empty)
import Suihei.Text.Ancillary.Statement (parseStatement)

import Text.Parsec.Prim (runParserT)

import Data.Functor.Identity (
    Identity (Identity))

parseLn :: String -> Either String Statement
parseLn ln = case runParserT parseStatement empty "" ln of
    Identity (Left err) -> Left (show err)
    Identity (Right st) -> Right st