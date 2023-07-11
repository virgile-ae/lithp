module Lex (Lex.lex) where

import Data.Char (isDigit, isLetter, isNumber, isSpace)
import Tokens (TokenKind (..))

isSymbolStart :: Char -> Bool
isSymbolStart ch = isLetter ch || ch `elem` "!%*+-/<=>?_"

isSymbolRest :: Char -> Bool
isSymbolRest ch = isSymbolStart ch || isNumber ch || ch == '\''

lex :: String -> [TokenKind]
lex = lex' []
  where
    -- EOF (base case)
    lex' acc "" = reverse (Eof : acc)
    -- Comments
    lex' acc (';' : xs) = lex' acc (dropWhile (/= '\n') xs)
    -- Whitespace character
    lex' acc (',' : xs) = lex' acc xs
    -- Important characters
    lex' acc ('#' : xs) = lex' (Hash : acc) xs
    lex' acc ('\'' : xs) = lex' (Quote : acc) xs
    lex' acc ('(' : xs) = lex' (LeftParen : acc) xs
    lex' acc (')' : xs) = lex' (RightParen : acc) xs
    lex' acc ('[' : xs) = lex' (LeftBracket : acc) xs
    lex' acc (']' : xs) = lex' (RightBracket : acc) xs
    lex' acc ('{' : xs) = lex' (LeftBrace : acc) xs
    lex' acc ('}' : xs) = lex' (RightBrace : acc) xs
    -- Keywords
    lex' acc (':' : xs) = lex' (Keyword (takeWhile isSymbolRest xs) : acc) $ dropWhile isSymbolRest xs
    -- Strings
    -- TODO: update string parser to include escape characters
    lex' acc ('"' : xs) = lex' (String (takeWhile (/= '"') xs) : acc) (tail $ dropWhile (/= '"') xs)
    lex' acc (x : xs)
      -- Whitespace
      | isSpace x = lex' acc xs
      -- Numbers
      | isDigit x = lex' (Number (read (x : takeWhile isDigit xs)) : acc) $ dropWhile isDigit xs
      -- Identifiers, booleans, and nil
      | isSymbolStart x =
          let val = x : takeWhile isSymbolRest xs
           in let tok
                    | val == "true" = Bool True
                    | val == "false" = Bool False
                    | val == "nil" = Nil
                    | otherwise = Symbol val
               in lex' (tok : acc) $ dropWhile isSymbolRest xs
      | otherwise = lex' (IllegalChar x : acc) xs
