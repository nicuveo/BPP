{-# LANGUAGE ParallelListComp #-}

module Assembler where

import           Control.Monad
import           Data.Function
import           Data.List       as L
import           Data.List.Split (chunksOf)
import qualified Data.Map        as M
import           Text.Printf

import           Compiler
import           Grammar
import           Object
import           Types


assembleVerbosely :: Monad m => ObjectMap -> m String
assembleVerbosely objs = unlines <$> expandFunc 0 mainFunc []
  where mainFunc = extractFun objs "main"
        postProd = (chunkify =<<) . groupBy bothCode . lines . concat
        bothCode = (&&) `on` (not . isPrefixOf "  ")
        chunkify s
          | "  " `isPrefixOf` concat s = s
          | otherwise                  = chunksOf 100 $ concat s
        indent   = map ("  " ++)
        render v = filter (`notElem` "+-[],.<>") $ intercalate "; " [n ++ "=" ++ show x | (n,x) <- v]
        expandFunc :: Monad m => Int -> Function -> [(String, Value)] -> m [String]
        expandFunc level func args = do
          when (level > 99) $ fail "stack too deep"
          i <- postProd <$> mapM expandInst (funcBody func args)
          let header = printf "%s%s" (funcName func) $
                if null args then "" else "(" ++ render args ++ ")"
          return $ if funcInline func
                   then i
                   else header : i
          where expandInst :: Monad m => WithPos Instruction -> m String
                expandInst (WithPos _ _ _ (RawBrainfuck r)) = return r
                expandInst (WithPos _ _ _ (FunctionCall g v)) = do
                  let callee = extractFun objs g
                      scope  = M.union (M.fromList $ fmap (WithPos undefined undefined undefined . ValueObject) <$> args) objs
                      params = [ (name, either (const $ error "FIXME") id $ eval scope kind expr)
                               | (kind, name) <- funcArgs callee
                               | expr         <- v
                               ]
                  result <- expandFunc (level+1) callee params
                  return $ if funcInline callee
                           then dropWhileEnd (== '\n') $ unlines result
                           else unlines $ "" : indent result
                expandInst (WithPos _ _ _ (Loop b)) = do
                  i <- indent . postProd <$> mapM expandInst b
                  return $ printf "\n[\n%s]\n" $ unlines i
                expandInst (WithPos _ _ _ (While c b)) = do
                  t <- mapM expandInst c
                  i <- mapM expandInst b
                  return $ printf "%s\n[[-]<\n%s]<\n" (unlines $ postProd t) $ unlines $ indent $ postProd $ i ++ t
                expandInst (WithPos _ _ _ (If c b)) = do
                  t <- unlines .          postProd <$> mapM expandInst c
                  i <- unlines . indent . postProd <$> mapM expandInst b
                  return $ printf "%s\n[[-]<\n%s>[-]]<\n" t i


-- denseFormatter :: Formatter
-- denseFormatter = Formatter fRaw fBFunc fAFunc fPost (const "[") (const "]")
--   where fRaw   _       = id
--         fBFunc _ _ _   = ""
--         fAFunc _ _ _   = ""
--         fPost          = unlines . chunksOf 100
--
--
-- assemble :: Monad m => Formatter -> ObjectMap -> m String
-- assemble f objs = formatPostProcess f <$> expandFunc f objs 0 mainFunc []
--   where mainFunc = extractFun objs "main"
--
-- expandFunc :: Monad m => Formatter -> ObjectMap -> Int -> Function -> [(String, Value)] -> m String
-- expandFunc f objs l func args = do
--   let b = formatBeforeFunction f l func args
--       a = formatAfterFunction  f l func args
--   i <- mapM (expandInst l) $ funcBody func
--   return $ b ++ concat i ++ a
--   where expandInst level (RawBrainfuck r) = return $ formatRawBrainfuck f level r
--         expandInst level (FunctionCall g v) = do
--           let callee = extractFun objs g
--               scope  = M.union (M.fromList $ fmap ValueObject <$> args) objs
--           params <- sequence [ (name,) <$> eval scope kind expr
--                              | (kind, name) <- funcArgs callee
--                              | expr         <- v
--                              ]
--           if funcInline callee
--             then expandFunc f objs  level    callee  params
--             else expandFunc f objs (level+1) callee  params
--         expandInst level (Loop b) = do
--           let o = formatLoopOpen  f level
--               c = formatLoopClose f level
--           i <- mapM (expandInst $ level+1) b
--           return $ o ++ concat i ++ c

extractFun :: ObjectMap -> Name -> Function
extractFun = getFun ... (M.!)
  where getFun (WithPos _ _ _ (FunctionObject f)) = f
        getFun _ = error "should never happen"
