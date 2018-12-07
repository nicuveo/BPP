{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections    #-}

module Assembler where

import           Data.List.Split (chunksOf)
import qualified Data.Map        as M

import           Compiler
import           Grammar
import           Object
import           Types




data Formatter = Formatter { formatRawBrainfuck   :: Int -> String -> String
                           , formatBeforeFunction :: Int -> Function -> [(String, Value)] -> String
                           , formatAfterFunction  :: Int -> Function -> [(String, Value)] -> String
                           , formatPostProcess    :: String -> String
                           }


verboseFormatter :: Formatter
verboseFormatter = Formatter fRaw fBFunc fAFunc fPost
  where padding level = [1..level] >> "  "
        fRaw   level rawbf  = unlines $ (padding level ++) <$> chunksOf 100 rawbf
        fAFunc _     _    _ = ""
        fBFunc level func v
          | funcInline func = ""
          | otherwise       = padding level ++ funcName func ++ if null v
                                                                then "\n"
                                                                else "(" ++ render v ++ ")\n"
        fPost = id
        render v = filter (`notElem` "+-[],.<>") $ unwords [n ++ "=" ++ show x | (n,x) <- v]

denseFormatter :: Formatter
denseFormatter = Formatter fRaw fBFunc fAFunc fPost
  where fRaw   _       = id
        fBFunc _ _ _   = ""
        fAFunc _ _ _   = ""
        fPost          = unlines . chunksOf 100


assemble :: Monad m => Formatter -> ObjectMap -> m String
assemble f objs = formatPostProcess f <$> expandFunc f objs 0 mainFunc []
  where mainFunc = extractFun objs "main"

expandFunc :: Monad m => Formatter -> ObjectMap -> Int -> Function -> [(String, Value)] -> m String
expandFunc f objs level func args = do
  let b = formatBeforeFunction f level func args
      a = formatAfterFunction  f level func args
  i <- mapM expandInst $ funcBody func
  return $ b ++ concat i ++ a
  where expandInst (RawBrainfuck r) = return $ formatRawBrainfuck f level r
        expandInst (FunctionCall g v) = do
          let callee = extractFun objs g
              scope  = M.union (M.fromList $ fmap ValueObject <$> args) objs
          params <- sequence [ (name,) <$> eval scope kind expr
                             | (kind, name) <- funcArgs callee
                             | expr         <- v
                             ]
          if funcInline callee
            then expandFunc f objs  level    callee  params
            else expandFunc f objs (level+1) callee  params

extractFun :: ObjectMap -> Name -> Function
extractFun = getFun ... (M.!)
  where getFun (FunctionObject f) = f
        getFun _                  = error "should never happen"
