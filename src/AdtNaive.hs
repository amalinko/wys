module AdtNaive where

data Term
  = Lit Int
  | Add Term Term
  deriving (Show)

eval :: Term -> Int
eval (Lit x) = x
eval (Add x y) = eval x + eval y

run :: IO ()
run = do
  let expression = Add (Add (Lit 2) (Lit 2)) (Lit 6)
  putStrLn $ show expression
  putStrLn $ show $ eval expression
