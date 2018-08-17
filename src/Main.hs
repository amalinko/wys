module Main where

import TypedInterpreters

main :: IO ()
main = do
  putStrLn "ok"
  putStrLn $ view $ exp1 22
  putStrLn $ show $ calculate $ exp1 22


