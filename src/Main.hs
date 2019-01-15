{-# LANGUAGE TupleSections #-}

module Main where

import           Data.List
import           System.Directory
import           System.Environment
import           System.Exit

import           Assembler
import           BuiltIn
import           Compiler



fileResolver :: String -> IO (Maybe (String, String))
fileResolver "Prelude" = Just . ("Prelude.bs",) <$> preludeFile
fileResolver filename  = do
  let rfn = if ".bs" `isSuffixOf` filename
            then filename
            else filename ++ ".bs"
  dfe <- doesFileExist rfn
  if dfe
    then return Nothing
    else Just . (rfn,) <$> readFile rfn


help :: IO a
help = putStrLn "usage: bfs [-O] file.bs" >> exitFailure

options :: IO (Bool, String)
options = do
  args <- getArgs
  case args of
    ["-O", f] -> return (True, f)
    [      f] -> return (False, f)
    _         -> help

main :: IO ()
main = do
  (dense, source) <- options
  (diags, mObjs)  <- compile fileResolver source
  mapM_ print diags
  case mObjs of
    Nothing -> putStrLn "Aborting."
    Just o  -> do
      let assembler = if dense then assembleDensely else assembleVerbosely
      putStr =<< either error return (assembler o)
