module Main where


import           Assembler
import           Compiler


main :: IO ()
main = do
  objs  <- either error return $ runCompiler testResolver "interactive"
  bfout <- either error return $ assembleVerbosely objs
  putStr bfout



testResolver :: String -> Either String String
testResolver "interactive" = return testData
testResolver "other"       = return otherData
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
           \\n\
           \\nconst I s1 = 3\
           \\nconst C s2 = 0x42\
           \\nconst C s3 = 'x'\
           \\nconst S s4 = \"test\"\
           \\n\
           \\ndef IMPURE INLINE  foo() [] -> [] { ++--++ }\n\
           \\ndef IMPURE baz(I a, C b, S c) [] -> [] { foo [-] foo() }\
           \\ndef main() { foo baz(s3, 42, \"test\") foo }\
           \\n"
