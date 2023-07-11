{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parse (parse) where

import qualified Ast
import Tokens (TokenKind)
import qualified Tokens

consume :: TokenKind -> ([TokenKind] -> Maybe (TokenKind, [TokenKind]))
consume tok = consume'
  where
    consume' [] = Nothing
    consume' (x : xs)
      | x == tok = Just (x, xs)
      | otherwise = Nothing

many :: ([TokenKind] -> Maybe (Ast.Node, [TokenKind])) -> ([TokenKind] -> Maybe ([Ast.Node], [TokenKind]))
many p = many' []
  where
    many' acc [] = Just (reverse acc, [])
    many' acc tokens =
      case (p tokens, acc) of
        -- No nodes were successfully parsed so it counts as a fail
        (Nothing, []) -> Nothing
        -- At least one node was successfully parsed, so it counts as a success
        (Nothing, acc) -> Just (reverse acc, tokens)
        (Just (node, rest), acc) -> many' (node : acc) rest

(<%>) :: Monad m => (t -> Maybe a) -> (t -> m a) -> t -> m a
(<%>) l r toks =
  case l toks of
    Just x -> return x
    Nothing -> r toks

parseValue :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseValue (Tokens.Bool x : xs) = Just (Ast.Bool x, xs)
parseValue (Tokens.Keyword x : xs) = Just (Ast.Keyword x, xs)
parseValue (Tokens.Nil : xs) = Just (Ast.Nil, xs)
parseValue (Tokens.Number x : xs) = Just (Ast.Number x, xs)
parseValue (Tokens.String x : xs) = Just (Ast.String x, xs)
parseValue (Tokens.Symbol x : xs) = Just (Ast.Symbol x, xs)
parseValue [Tokens.Eof] = fail "unexpected end of file while parsing value"
parseValue _ = fail "unable to parse value"

-- Parses a list: ()
parseList :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseList (Tokens.LeftParen : xs) = do
  (listContents, rest) <- parseExprs xs
  (_, rest) <- consume Tokens.RightParen rest
  return (Ast.List listContents, rest)
parseList _ = fail "unable to parse list"

-- Parses a vector: []
parseVec :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseVec (Tokens.LeftBracket : xs) = do
  (listContents, rest) <- parseExprs xs
  (_, rest) <- consume Tokens.RightBracket rest
  return (Ast.Vector listContents, rest)
parseVec _ = fail "unable to parse vector"

-- Parses a dict: {}
-- Need to parse pairs, with keys either being ints, keywords, or strings
parseDict :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseDict tokens = fail "TODO: implement"

-- Parses the shorthand for an anonymous function: #()
parseAnonFn :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseAnonFn tokens = fail "TODO: implement"

-- Parses a set: #{}
parseSet :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseSet tokens = fail "todo implement"

parseDispatchForm :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseDispatchForm = parseAnonFn <%> parseSet

-- Parses a list, vector, set, dict, anonymous function, or value
parseExpr :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseExpr = parseValue <%> parseList <%> parseVec <%> parseDispatchForm

parseExprs :: [TokenKind] -> Maybe ([Ast.Node], [TokenKind])
parseExprs = many parseExpr

parse :: [TokenKind] -> Maybe ([Ast.Node], [TokenKind])
parse = parseExprs
