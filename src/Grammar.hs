module Grammar where



-- imports

import           Data.List
import           Text.Printf

import           Types



-- grammar

type Program = [WithPos Statement]

data Statement = Include      Filename
               | Comment      Raw
               | ConstantDecl Constant
               | FunctionDecl Function
               deriving (Show)

data Instruction = FunctionCall Name [Expression]
                 | RawBrainfuck Raw
                 | If           [WithPos Instruction] [WithPos Instruction]
                 | Loop         [WithPos Instruction]
                 | While        [WithPos Instruction] [WithPos Instruction]
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
                         , funcBody   :: [(String, Value)] -> [WithPos Instruction]
                         }


instance Show Function where
  show f = printf "{%sfunction %s(%s) %s -> %s}" q n a i o
    where q = unwords $ ["impure" | not $ funcPure f] ++ ["inline" | funcInline f] ++ [""]
          n = funcName f
          a = intercalate ", " $ show <$> funcArgs f
          i = show $ funcInput f
          o = show $ funcOutput f


isImpure :: Instruction -> Bool
isImpure (FunctionCall _ _) = False
isImpure (RawBrainfuck _  ) = True
isImpure (Loop         b  ) = anyImpure (posThing <$> b)
isImpure (If           c b) = anyImpure (posThing <$> c) || anyImpure (posThing <$> b)
isImpure (While        c b) = anyImpure (posThing <$> c) || anyImpure (posThing <$> b)

anyImpure :: [Instruction] -> Bool
anyImpure = any isImpure



-- values

data Type = BFInt
          | BFChar
          | BFString
          | BFBool
          deriving (Eq)

data Value = VInt Int
           | VChar Char
           | VString String
           | VBool Bool
           deriving (Eq)

type Variable  = (Type, Name)


instance Show Type where
  show BFInt    = "Int"
  show BFChar   = "Char"
  show BFString = "String"
  show BFBool   = "Bool"

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
