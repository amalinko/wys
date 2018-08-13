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
  deriving (Eq)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (LString s) = "\"" ++ s ++ "\""
showVal (LBool True) = "#t"
showVal (LBool False) = "#f"
showVal (LNumber n) = show n
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Atom name) = name

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

parseExpression :: Parser LispVal
parseExpression =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseListOrDottedList

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x

parseList :: Parser LispVal
parseList = fmap List (sepBy parseExpression spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpression spaces
  tail <- char '.' >> spaces >> parseExpression
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpression
  return $ List [Atom "quote", x]

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escaped <|> noneOf "\"\\")
  char '"'
  return $ LString x
  where
    escaped = char '\\' >> oneOf "\\\""

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
