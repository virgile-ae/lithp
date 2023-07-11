module Main (main) where

import qualified Ast
import qualified Lex
import qualified Parse

main :: IO ()
main = do
  let tokens = Lex.lex "(hello :tomato 123 true false nil (banana))"
  print tokens
  let Just (ast, rest) = Parse.parse tokens
  putStrLn $ unlines $ map Ast.prettify ast
