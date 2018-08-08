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
  case parse parseExpression "lisp" input of
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

parseExpression :: Parser LispVal
parseExpression = parseString <|> parseAtom <|> parseNumber

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ LString x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $
    case (first : rest) of
      "#t" -> LBool True
      "#f" -> LBool False
      other -> Atom other

parseNumber :: Parser LispVal
parseNumber = fmap (LNumber . read) (many digit)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space
