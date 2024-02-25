module Main where

import qualified MyLib (someFunc)
import qualified Json (parse)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  print Json.parse
