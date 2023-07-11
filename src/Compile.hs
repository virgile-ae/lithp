module Compile (compile) where

import Ast
import Lex
import Parse
import Tokens

compile :: String -> Maybe ([Ast.Node], [Tokens.TokenKind])
compile = Parse.parse . Lex.lex
