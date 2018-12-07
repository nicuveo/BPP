{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar (
  Program,
  Statement(..),
  Instruction(..),
  Expression(..),
  Constant(..),
  Function(..),
  Type(..),
  Value(..),
  Variable,
  WithPos(..),
  parseProgram,
  typeof,
  )where



-- imports

import           Data.List
import           Numeric
import           Text.Parsec
import           Text.Parsec.String

import           Types



-- grammar

type Program = [Statement]

data Statement = Include      Filename
               | Comment      Raw
               | ConstantDecl Constant
               | FunctionDecl Function
               deriving (Show)

data Instruction = FunctionCall Name [Expression]
                 | RawBrainfuck Raw
                 | Loop         [Instruction]
                 | While        [Instruction] [Instruction]
                 deriving (Show)

data Expression = ConstantName  Name
                | LiteralString String
                | LiteralChar   Char
                | LiteralInt    Int
                deriving (Show)


data Constant = Constant { constType :: Type
                         , constName :: Name
                         , constExpr :: Expression
                         } deriving (Show)

data Function = Function { funcName   :: Name
                         , funcPure   :: Bool
                         , funcInline :: Bool
                         , funcArgs   :: [Variable]
                         , funcInput  :: [Type]
                         , funcOutput :: [Type]
                         , funcBody   :: [Instruction]
                         } deriving (Show)

data WithPos a = WithPos { posLine   :: Int
                         , posColumn :: Int
                         , posThing  :: a
                         }



-- values

data Type = BFInt
          | BFChar
          | BFString
          | BFBool
          deriving (Show, Eq)

data Value = VInt Int
           | VChar Char
           | VString String
           | VBool Bool
           deriving (Eq)

type Variable  = (Type, Name)


instance Show Value where
  show (VInt    i) = show i
  show (VChar   c) = show c
  show (VString s) = show s
  show (VBool   b) = show b

typeof :: Value -> Type
typeof (VInt    _) = BFInt
typeof (VChar   _) = BFChar
typeof (VString _) = BFString
typeof (VBool   _) = BFBool



-- parsing

parseProgram :: Filename -> String -> Program
parseProgram = either (error . show) id ... parse program


program = statement `sepEndBy` many newline <* eof

statement = oneof
  [ include
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
  spaces
  e <- expression
  return $ ConstantDecl $ Constant t n e

function = do
  symbol "def"
  k <- oneof (symbol <$> keywords) `sepEndBy` spaces
  let p = impure `notElem` k
      l = inline `elem` k
  n <- name
  a <- betweenParens   $ variable `sepBy` symbol ","
  (i, o) <- option ([], []) $ do
    i <- betweenBrackets $ typename `sepBy` symbol ","
    symbol "->"
    o <- betweenBrackets $ typename `sepBy` symbol ","
    return (i, o)
  b <- betweenCurlies  $ instruction `sepEndBy` spaces
  return $ FunctionDecl $ Function n p l a i o b


instruction = oneof
  [ functionCall
  , rawBrainfuck
  , loop
  , while
  ] -- <* try comment

functionCall = do
  n <- name
  a <- option [] $ betweenParens $ expression `sepBy` symbol ","
  return $ FunctionCall n a

rawBrainfuck = do
  let valid = oneof $ char <$> brainfuckChars
  s <- many1 valid `sepEndBy1` spaces
  return $ RawBrainfuck $ concat s

loop = do
  symbol "loop"
  b <- betweenCurlies $ instruction `sepEndBy` spaces
  return $ Loop b

while = do
  symbol "while"
  c <- betweenParens  $ instruction `sepEndBy` spaces
  b <- betweenCurlies $ instruction `sepEndBy` spaces
  return $ While c b


expression = oneof
  [ constantName
  , literalString
  , literalChar
  , literalHex
  , literalDec
  ]

constantName = ConstantName <$> name

literalString = LiteralString <$> betweenQuotes (many1 $ noneOf "\"")

literalChar = do
  c <- between (char '\'') (char '\'') $ noneOf "'"
  return $ LiteralChar c

literalHex = do
  string "0x"
  n <- many1 hexDigit
  -- FIXME: handle no parse
  return $ LiteralInt $ fst $ head $ readHex n

literalDec = do
  n <- many1 digit
  -- FIXME: handle no parse
  return $ LiteralInt $ read n


typename = oneof [pstring, pint, pchar]
  where pstring = BFString <$ char 'S'
        pint    = BFInt    <$ char 'I'
        pchar   = BFChar   <$ char 'C'

name = (:) <$> letter <*> many alphaNum

variable = do
  t <- typename
  spaces
  n <- name
  return (t, n)



-- helpers

symbol :: String -> Parser String
symbol s = string s <* spaces


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

inline = "INLINE"
impure = "IMPURE"
keywords = sort [inline, impure]

brainfuckChars = "+-,.<>[]"
