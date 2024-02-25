module Main where

import qualified MyLib (someFunc)
import qualified Json (parse, JValue(..))
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  print $ fromMaybe Json.JNull $ Json.parse "[\"ahoj\"]"
