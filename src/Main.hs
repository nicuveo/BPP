module Main where


import           Assembler
import           BuiltIn
import           Compiler


main :: IO ()
main = do
  (diags, mObjs) <- runCompiler testResolver "interactive"
  mapM_ print diags
  case mObjs of
    Nothing -> putStrLn "Aborting."
    Just o  -> do
      bfout <- either error return $ assembleVerbosely o
      putStr bfout



testResolver :: String -> IO String
testResolver "interactive" = return testData
testResolver "other"       = return otherData
testResolver "Prelude"     = preludeFile
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
           \\ndef main() { foo baz(s3, 42, \"test\") foo if(true) { foo } endl }\
           \\n"
