module Bytecode where

-- import Data.Int (Int8)
import Data.List (group)

import Ast (Node)
import qualified Ast
import Data.Int (Int8)

runLengthEncode :: (Eq a) => [a] -> [(a, Int)]
runLengthEncode = map (\x -> (head x, length x)) . group

data VirtualMachine = VirtualMachine
  { stack :: [Int8]

  }

call       = 0  :: Int8
loadGlobal = 1  :: Int8
loadTemp   = 2  :: Int8
makeFunc   = 3  :: Int8
pop        = 4  :: Int8
push       = 5  :: Int8
store      = 6  :: Int8

data InterpretResult
  = Ok
  | CompileError
  | RuntimeError
  deriving (Eq, Show)


compileToOpCodes :: [Node] -> [Int8]
compileToOpCodes (Ast.List x : xs) = []
compileToOpCodes [] = []

compileNode :: Node -> [Int8]
compileNode x@(Ast.List _) = compileList x


compileList :: Node -> [Int8]
compileList (Ast.List (fn : args)) =
  let fn' = compileNode fn in
      [] -- TODO


interpretOpCodes :: [Int8] -> InterpretResult
interpretOpCodes _ = CompileError
