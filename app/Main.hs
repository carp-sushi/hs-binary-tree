module Main (main) where

import Data.Tree

main :: IO ()
main = do
  let numbers = [4, 2, 1, 3, 7, 6, 9]
      tree = mkTree numbers
      (Inverted inverted) = invert tree

  putStrLn "Numbers"
  print numbers
  putStrLn "Tree"
  print tree
  putStrLn "<+"
  print $ 9 <+ 6 <+ 7 <+ 3 <+ 1 <+ 2 <+ 4 <+ Nil
  putStrLn "+>"
  print $ Nil +> 4 +> 2 +> 1 +> 3 +> 7 +> 6 +> 9

  putStrLn "Inverted Tree"
  print inverted
  putStrLn "Inverted As List"
  print $ flatten inverted

  putStrLn "Insert 5, 8"
  print $ (insert 8 . insert 5) tree
  putStrLn "Search 7"
  print $ search 7 tree
  putStrLn "Remove 7"
  print $ remove 7 tree

  putStrLn "Max"
  print $ max_ tree
  putStrLn "Min"
  print $ min_ tree

  putStrLn "Tree x 2"
  print $ fmap (* 2) tree
