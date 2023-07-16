module Ast (Node (..), prettify) where

import Data.List (intercalate)

data Node
  = AnonFn [Node]
  | Bool Bool
  | Dict [Node]
  | Keyword String
  | List [Node]
  | Nil
  | Number Double
  | Set [Node]
  | String String
  | Symbol String
  | Vector [Node]
  deriving (Show, Eq)

leftPad :: Int -> String -> String
leftPad n e = replicate n ' ' ++ e

tabWidth :: Int
tabWidth = 2

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where
    (as, bs) = splitAt n xs

prettify :: Node -> String
prettify = prettify' 0
  where
    prettify' d Nil = leftPad d "nil"
    prettify' d (Bool b) = leftPad d $ show b
    prettify' d (Number n) = leftPad d $ show n
    prettify' d (String s) = leftPad d ('"' : s ++ "\"")
    prettify' d (Keyword k) = leftPad d (':' : k)
    prettify' d (Symbol s) = leftPad d s
    prettify' d (List xs) = prettyList d ("(", ")") xs
    prettify' d (Vector xs) = prettyList d ("[", "]") xs
    prettify' d (AnonFn xs) = prettyList d ("#(", ")") xs
    prettify' d (Set xs) = prettyList d ("#{", "}") xs

    prettify' d (Dict []) = leftPad d "{}"
    prettify' d (Dict [k, v]) = leftPad d ("{" ++ prettify' 0 k ++ " " ++ prettify' 0 v ++ "}")
    prettify' d (Dict (k : v : xs)) =
      let lhs = leftPad d ("{" ++ prettify' 0 k ++ prettify' 0 v ++ "\n") in
      let inner = intercalate "\n"
                  $ map (leftPad (tabWidth * d + 2) . unwords . map (prettify' 0))
                  $ splitEvery 2 xs in
      lhs ++ inner ++ "}"

    prettyList d (l, r) [] = leftPad d (l ++ r)
    prettyList d (l, r) [x] = leftPad d (l ++ prettify' 0 x ++ r)
    prettyList d (l, r) (x : xs) =
      let inner = intercalate "\n" $ map (prettify' $ d + length l) xs in
      leftPad d (l ++ prettify' 0 x ++ "\n") ++ inner ++ r
