module Main where

import           Data.List
import           System.Directory


import           Assembler
import           BuiltIn
import           Compiler



fileResolver :: String -> IO (Maybe String)
fileResolver "Prelude" = Just <$> preludeFile
fileResolver filename  = do
  let rfn = if ".bs" `isSuffixOf` filename
            then filename
            else filename ++ ".bs"
  dfe <- doesFileExist rfn
  if dfe
    then return Nothing
    else Just <$> readFile rfn



main :: IO ()
main = do
  (diags, mObjs) <- runCompiler testResolver "interactive"
  mapM_ print diags
  case mObjs of
    Nothing -> putStrLn "Aborting."
    Just o  -> do
      putStr =<< either error return (assembleVerbosely o)
      putStr =<< either error return (assembleDensely o)



testResolver :: String -> IO (Maybe String)
testResolver "interactive" = return $ Just testData
testResolver "other"       = return $ Just otherData
testResolver "Prelude"     = Just <$> preludeFile
testResolver _             = error "NIH"

oldTestData :: String
oldTestData = "// Test\
              \\ninclude \"other\"\
              \\n\
              \\nconst I s = 3\
              \\nconst C s2 = 0x42\
              \\n\
              \\ndef IMPURE foo() [] -> [] { ++--++ BAR }\n\
              \\ndef foo2(I x, C y) [I,I] -> [I] { BAZ(42, x) }\
              \\n"

otherData :: String
otherData = "def IMPURE BAR() [C] -> [] {}"

testData :: String
testData = "// Test\
           \\nconst I s1 = 3\
           \\nconst C s2 = 0x42\
           \\nconst C s3 = 'x'\
           \\nconst S s4 = \"test\"\
           \\n\
           \\ndef impure inline true() [] -> [B] { >+ }\n\
           \\ndef impure inline foo() [] -> [] { ++--++ }\n\
           \\ndef impure baz(I a, C b, S c) [] -> [] { foo [-] foo() }\
           \\ndef main() { foo baz(s3, 42, \"test\") foo pushc(220) if(true) { set(200) } prints(\"toto\\n\") popc }\
           \\n"
