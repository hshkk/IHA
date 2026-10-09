module IHA.Text.Parser (
    execute
) where

import IHA.Control.Interpreter (interpret)
import IHA.Data.Abstract (
    Statement (..))
import IHA.Text.Ancillary.Monad (Parser)
import IHA.Text.Ancillary.State (empty, reset)
import IHA.Text.Ancillary.Statement (parseStatement)

import Text.Parsec.Prim (runParserT)
import Text.ParserCombinators.Parsec hiding (Parser)

import Control.Monad (void)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (intercalate)
import Data.Maybe (catMaybes)

execute :: String  -> IO ()
execute file = do
    prog' <- readFile file
    -- Removes the comments from the string derived from the input file.
    -- In the future, this should be written using the language definition.
    case runIdentity $ runParserT ignore empty file prog' of
        Right prog -> 
            -- Then, runs the parser to generate the abstract syntax tree.
            case runIdentity $ runParserT parseFile empty file prog of
                Right res -> interpret res
                Left  err -> print err
        Left  err -> print err

parseFile :: Parser [Statement]
parseFile = do
    sts <- many ((>>) reset . return =<< parseLn)
    eof
    return (catMaybes sts)

parseLn :: Parser (Maybe Statement)
parseLn = (Just <$> parseStatement) <|> (eol >> return Nothing)

comment :: Parser ()
comment = (string "--" >> manyTill anyChar eol >> spaces >> return ()) 
      <|> (string "{-" >> manyTill anyChar (string "-}") >>  spaces >> return ())

ignore :: Parser String
ignore = do
    optional comment
    sts <- sepBy extract comment
    optional comment
    return (intercalate "" sts)
    where extract :: Parser String
          extract = manyTill anyChar (lookAhead (comment <|> eof))

eol :: Parser ()
eol = void (string "\n" <|> string "\r\n")