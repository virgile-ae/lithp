module Main (main) where

import Ast
import Lex
import Parse

main :: IO ()
main = do
  let tokens = Lex.lex "#{hello :tomato 123 true false nil (banana #(a b c d e) 2 3 4)}"
  print tokens
  let Just (ast, rest) = Parse.parse tokens
  putStrLn $ unlines $ map Ast.prettify ast
