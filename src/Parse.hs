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
      | x == tok  = Just (x, xs)
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

orElse :: (Monad m) => (t -> Maybe a) -> (t -> m a) -> (t -> m a)
orElse l r toks =
  case l toks of
    Just x  -> return x
    Nothing -> r toks

-- parseValue :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseValue :: (MonadFail m) => [TokenKind] -> m (Ast.Node, [TokenKind])
parseValue (Tokens.Bool x : xs)    = return (Ast.Bool x, xs)
parseValue (Tokens.Keyword x : xs) = return (Ast.Keyword x, xs)
parseValue (Tokens.Nil : xs)       = return (Ast.Nil, xs)
parseValue (Tokens.Number x : xs)  = return (Ast.Number x, xs)
parseValue (Tokens.String x : xs)  = return (Ast.String x, xs)
parseValue (Tokens.Symbol x : xs)  = return (Ast.Symbol x, xs)
parseValue [Tokens.Eof] = fail "unexpected end of file while parsing value"
parseValue _ = fail "unable to parse value"

-- Parses a list: ()
parseList :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseList (Tokens.LeftParen : xs) = do
  (contents, rest) <- parseExprs xs
  (_, rest) <- consume Tokens.RightParen rest
  return (Ast.List contents, rest)
parseList _ = fail "unable to parse list"

-- Parses a vector: []
parseVec :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseVec (Tokens.LeftBracket : xs) = do
  (contents, rest) <- parseExprs xs
  (_, rest) <- consume Tokens.RightBracket rest
  return (Ast.Vector contents, rest)
parseVec _ = fail "unable to parse vector"

-- Parses a dict: {}
-- Need to parse pairs, with keys either being ints, keywords, or strings
parseDict :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseDict (Tokens.LeftBrace : xs) = fail "TODO: implement"
parseDict tokens = fail "TODO: implement"

-- Parses the shorthand for an anonymous function: #()
parseAnonFn :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseAnonFn (Tokens.Hash : xs) =
  case parseList xs of
    Just (Ast.List val, rest) -> Just (Ast.AnonFn val, rest)
    Just _ -> Nothing
    Nothing -> Nothing
parseAnonFn [Tokens.Eof] = fail "unexpected end of file"
parseAnonFn (x : _) = fail "TODO: implement"
parseAnonFn [] = fail "unable to parse anonymous function"

-- Parses a set: #{}
parseSet :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseSet (Tokens.Hash : Tokens.LeftBrace : xs) = do
  (members, rest) <- parseExprs xs
  (_, rest) <- consume Tokens.RightBrace rest
  return (Ast.Set members, rest)
parseSet tokens = fail "todo implement"

parseDispatchForm :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseDispatchForm = parseAnonFn `orElse` parseSet

-- Parses a list, vector, set, dict, anonymous function, or value
parseExpr :: [TokenKind] -> Maybe (Ast.Node, [TokenKind])
parseExpr = parseValue `orElse` parseList `orElse` parseVec `orElse` parseDispatchForm

parseExprs :: [TokenKind] -> Maybe ([Ast.Node], [TokenKind])
parseExprs = many parseExpr

parse :: [TokenKind] -> Maybe ([Ast.Node], [TokenKind])
parse = parseExprs
