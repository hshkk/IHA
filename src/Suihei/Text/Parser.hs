module Suihei.Text.Parser (
    execute
) where

import Suihei.Control.Interpreter (interpret)
import Suihei.Data.Abstract (
    Statement  (..))
import Suihei.Text.Ancillary.Monad (Parser)
import Suihei.Text.Ancillary.State (empty, reset)
import Suihei.Text.Ancillary.Statement (parseStatement)

import Text.Parsec.Prim (runParserT)
import Text.ParserCombinators.Parsec (eof, many, (<|>), string)

import Control.Monad (void)
import Data.Functor.Identity (
    Identity (Identity))
import Data.Maybe (catMaybes)

execute :: String  -> IO ()
execute file = do
    prog <- readFile file
    case runParserT parseFile empty file prog of
        Identity (Right res) -> do
            interpret res
        Identity (Left  err) -> print err

parseFile :: Parser [Statement]
parseFile = do
    sts <- many ((>>) reset . return =<< parseLn)
    eof
    return (catMaybes sts)

parseLn :: Parser (Maybe Statement)
parseLn = (Just <$> parseStatement) <|> (eol >> return Nothing)

eol :: Parser ()
eol = void (string "\n" <|> string "\r\n")