{-# LANGUAGE GADTs #-}

module Gadt where

data Term a where
  Lit :: Int -> Term Int
  Add :: Term Int -> Term Int -> Term Int
  Gt :: Term Int -> Term Int -> Term Bool
  IfElse :: Term Bool -> Term a -> Term a -> Term a

eval :: Term a -> a
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Gt x y) = eval x > eval y
eval (IfElse condition x y) =
  if eval condition
    then eval x
    else eval y

showT :: Term a -> String
showT (Lit x) = show x
showT (Add x y) = "(" ++ showT x ++ " + " ++ showT y ++ ")"
showT (Gt x y) = showT x ++ " > " ++ showT y
showT (IfElse condition x y) = "if(" ++ showT condition ++ ") " ++ showT x ++ " else " ++ showT y

run :: IO ()
run = do
  let expression =
        IfElse (Gt (Lit 5) (Add (Lit 4) (Lit 2))) (Add (Lit 5) (Lit 6)) (Lit 8)
  putStrLn $ showT expression
  putStrLn $ show $ eval expression
