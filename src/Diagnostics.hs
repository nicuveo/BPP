module Diagnostics where


import           Grammar
import           Object
import           Types


type Diagnostic  = WithPos Error
type Diagnostics = [Diagnostic]

data Error = ConstantAlreadyDefinedError           String (WithPos Object)
           | FunctionAlreadyDefinedError           String (WithPos Object)
           | ArgumentNameShadowsObjectWarning      String (WithPos Object)
           | IntLiteralError                       Type Int
           | CharLiteralError                      Type Char
           | StringLiteralError                    Type String
           | ImplicitCastError                     Type Type
           | FunctionTypeDeclarationError          Function [Type]
           | FunctionCallWrongArgumentsNumberError Function Int
           | FunctionCallWrongArgumentTypeError    Function Variable Type
           | FunctionCallStackTypeError            Function [Type]
           | BlockIfNotStackNeutralError           ([Type], [Type])
           | BlockLoopNotStackNeutralError         ([Type], [Type])
           | BlockWhileNotStackNeutralError        ([Type], [Type])
           | ConditionWrongTypeError               ([Type], [Type])
           | ConditionWrongInstructionError        Instruction
           | ExpectedValueGotFunctionError         String (WithPos Object)
           | ExpectedFunctionGotValueError         String (WithPos Object)
           | PureFunctionsContainsImpureCodeError  String
           | ConstantNotFoundError                 String
           | FunctionNotFoundError                 String
           | DuplicateArgumentNamesError           [String]
           deriving (Show)

isError :: Diagnostic -> Bool
isError (WithPos _ _ _ (ArgumentNameShadowsObjectWarning _ _)) = False
isError _ = True
