module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (h:_) -> putStrLn $ readExpression h
    _ -> putStrLn "Should be one arg"

readExpression :: String -> String
readExpression input =
  case parse (parseLString) "lisp" input of
    Right val -> "Found value: " ++ show val
    Left err -> "No match: " ++ show err

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | LNumber Int
  | LString String
  | LBool Bool
  deriving (Eq, Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseLString :: Parser LispVal
parseLString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ LString x
