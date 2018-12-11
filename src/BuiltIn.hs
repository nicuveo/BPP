module BuiltIn where


import           Paths_BPP




preludeFile :: IO String
preludeFile = readFile =<< getDataFileName "src/Prelude.bs"
