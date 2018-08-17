{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TypedInterpreters where


class Calculator a where
  lit :: Int -> a
  add :: a -> a -> a
  neg :: a -> a -> a

instance Calculator String where
  lit = show
  add x y = "(" ++ x ++ " + " ++ y ++ ")"
  neg x y = "(" ++ x ++ " - " ++ y ++ ")"

instance Calculator Int where
  lit x = x
  add x y = x + y
  neg x y = x - y

exp1 :: Calculator a => Int -> a
exp1 x = neg (add (lit x) (lit 2)) (lit 10)

view :: String -> String
view = id

calculate :: Int -> Int
calculate = id
