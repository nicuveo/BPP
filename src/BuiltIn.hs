module BuiltIn where

import           Data.Char
import           Data.List as L
import qualified Data.Map  as M

import           Grammar
import           Object
import           Types

import           Paths_BPP


preludeFile :: IO String
preludeFile = readFile =<< getDataFileName "src/Prelude.bs"


builtinFunctions :: ObjectMap
builtinFunctions = foldl' insertFunc M.empty [pushc, pushi]
  where insertFunc m f = M.insert (funcName f) (dummyPos $ FunctionObject f) m


pushc :: Function
pushc = Function "pushc" False True [(BFChar, "x")] [] [BFChar] pushc_
  where pushc_ [("x", VChar c)] = [dummyPos $ RawBrainfuck $ '>' : replicate (ord c) '+']
        pushc_ _ = error "ICE"

pushi :: Function
pushi = Function "pushi" False True [(BFInt, "x")] [] [BFInt] pushi_
  where pushi_ [("x", VInt x)] = [dummyPos $ RawBrainfuck $ concat ['>' : replicate i '+' | i <- reverse $ decompose 3 x]]
        pushi_ _ = error "ICE"
        decompose n x = undefined

dummyPos :: a -> WithPos a
dummyPos = WithPos "<builtin>" 0 0
