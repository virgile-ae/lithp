module Tokens (TokenKind (..)) where

data TokenKind
  = Bool Bool -- Data literals
  | Number Double
  | String String
  | Keyword String
  | Symbol String
  | Nil
  | Hash -- Important characters
  | Quote
  | Underscore
  | LeftParen -- Delimiters
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Eof -- Other
  | IllegalChar Char
  deriving (Show, Eq)
