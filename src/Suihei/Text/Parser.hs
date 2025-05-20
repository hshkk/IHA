module Suihei.Text.Parser (
    execute
) where

import Suihei.Data.Abstract (
    Statement  (..))
import Suihei.Text.Ancillary.State (empty)
import Suihei.Text.Ancillary.Statement (parseStatement)

import Text.Parsec.Prim (runParserT)
import Text.ParserCombinators.Parsec (eof, many, (<|>), string)

import Data.Functor.Identity (
    Identity (Identity))
import Suihei.Control.Interpreter (interpret)
import Suihei.Text.Ancillary.Monad (Parser)
import Data.Maybe (catMaybes)
import Control.Monad (void)

execute :: String  -> IO ()
execute file = do
    prog <- readFile file
    case runParserT parseFile empty file prog of
        Identity (Right res) -> do
            interpret res
        Identity (Left  err) -> print err

parseFile :: Parser [Statement]
parseFile = do
    sts <- many parseLn
    eof
    return (catMaybes sts)

parseLn :: Parser (Maybe Statement)
parseLn = (Just <$> parseStatement) <|> (eol >> return Nothing)

eol :: Parser ()
eol = void (string "\n" <|> string "\r\n")