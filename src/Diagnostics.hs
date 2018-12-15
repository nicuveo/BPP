module Diagnostics where


import           Grammar
import           Object
import           Types


type Diagnostic  = WithLocation Error
type Diagnostics = [Diagnostic]

data Error = ConstantAlreadyDefinedError           String (WithLocation Object)
           | FunctionAlreadyDefinedError           String (WithLocation Object)
           | ArgumentNameShadowsObjectWarning      String (WithLocation Object)
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
           | ExpectedValueGotFunctionError         String (WithLocation Object)
           | ExpectedFunctionGotValueError         String (WithLocation Object)
           | PureFunctionsContainsImpureCodeError  String
           | ConstantNotFoundError                 String
           | FunctionNotFoundError                 String
           | DuplicateArgumentNamesError           [String]
           deriving (Show)

isError :: Diagnostic -> Bool
isError (WL _ (ArgumentNameShadowsObjectWarning _ _)) = False
isError _ = True
