{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parser (parseProgram) where



-- imports

import           Data.List
import           Numeric
import           Text.Parsec
import           Text.Parsec.String

import           Grammar
import           Types



-- program

parseProgram :: Filename -> String -> Program
parseProgram = either (error . show) id ... parse program

program = statement `sepEndBy` many newline <* eof



-- statement

statement = do
  pos <- getPosition
  addPosition pos <$> oneof [ include
                            , comment
                            , constant
                            , function
                            ]

include = do
  symbol "include"
  filename <- betweenQuotes $ many1 $ noneOf "\"\n\t"
  return $ Include filename

comment = do
  symbol "//"
  text <- many $ noneOf "\n"
  return $ Comment text

constant = do
  symbol "const"
  (t, n) <- variable
  spaces
  symbol "="
  e <- expression
  return $ ConstantDecl $ Constant t n e

function = do
  symbol "def"
  k <- oneof (string <$> keywords) `sepEndBy` many1 space
  let p = impure `notElem` k
      l = inline `elem` k
  n <- name
  a <- betweenParens $ variable `sepBy` symbol ","
  (i, o) <- option ([], []) $ do
    many space
    i <- betweenBrackets $ typename `sepBy` symbol ","
    symbol "->"
    many space
    o <- betweenBrackets $ typename `sepBy` symbol ","
    return (i, o)
  b <- betweenCurlies $ instruction `sepEndBy` spaces
  return $ FunctionDecl $ Function n p l a i o $ const b



-- instructions

instruction = do
  pos  <- getPosition
  addPosition pos <$> oneof [ rawBrainfuck
                            , ifb
                            , loop
                            , while
                            , functionCall
                            ]

functionCall = do
  n <- name
  a <- option [] $ try $ betweenParens $ expression `sepBy` symbol ","
  return $ FunctionCall n a

rawBrainfuck = do
  let valid = oneof $ char <$> brainfuckChars
  s <- many1 valid `sepEndBy1` spaces
  return $ RawBrainfuck $ concat s

ifb = do
  symbol "if"
  c <- betweenParens  $ instruction `sepEndBy` spaces
  b <- betweenCurlies $ instruction `sepEndBy` spaces
  return $ If c b

loop = do
  symbol "loop"
  b <- betweenCurlies $ instruction `sepEndBy` spaces
  return $ Loop b

while = do
  symbol "while"
  c <- betweenParens  $ instruction `sepEndBy` spaces
  b <- betweenCurlies $ instruction `sepEndBy` spaces
  return $ While c b



-- expressions

expression = oneof
  [ constantName
  , literalString
  , literalChar
  , literalHex
  , literalDec
  ]

constantName = ConstantName <$> name

literalString = LiteralString <$> betweenQuotes (many $ oneof [esct, escn, escq, noneOf "\""])
  where esct = char '\\' >> char 't' >> return '\t'
        escn = char '\\' >> char 'n' >> return '\n'
        escq = char '\\' >> char '"' >> return '"'

literalChar = do
  c <- between (char '\'') (char '\'') $ oneof [esct, escn, escq, noneOf "'"]
  return $ LiteralChar c
  where esct = char '\\' >> char 't'  >> return '\t'
        escn = char '\\' >> char 'n'  >> return '\n'
        escq = char '\\' >> char '\'' >> return '\''

literalHex = do
  string "0x"
  n <- many1 hexDigit
  -- FIXME: handle no parse
  return $ LiteralInt $ fst $ head $ readHex n

literalDec = do
  n <- many1 digit
  -- FIXME: handle no parse
  return $ LiteralInt $ read n



-- variables and types

typename = oneof [pstring, pint, pchar, pbool]
  where pstring = BFString <$ char 'S'
        pint    = BFInt    <$ char 'I'
        pchar   = BFChar   <$ char 'C'
        pbool   = BFBool   <$ char 'B'

name = (:) <$> oneof [char '_', letter] <*> many (oneof [alphaNum, char '_'])

variable = do
  t <- typename
  spaces
  n <- name
  return (t, n)



-- helpers

symbol :: String -> Parser String
symbol s = string s <* spaces


addPosition :: SourcePos -> a -> WithPos a
addPosition p = WithPos n l c
  where n = sourceName   p
        l = sourceLine   p
        c = sourceColumn p


oneof = foldl1 (<|>) . map try

quote        = char '"'
openParen    = symbol "("
openCurly    = symbol "{"
openBracket  = symbol "["
closeParen   = symbol ")"
closeCurly   = symbol "}"
closeBracket = symbol "]"

betweenQuotes   = between quote       quote
betweenParens   = between openParen   closeParen
betweenCurlies  = between openCurly   closeCurly
betweenBrackets = between openBracket closeBracket

inline = "inline"
impure = "impure"
keywords = sort [inline, impure]

brainfuckChars = "+-,.<>[]"
