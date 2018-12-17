module Object where



-- imports

import qualified Data.Map as M

import           Grammar
import           Module



-- object

data Object = FunctionObject Function
            | ValueObject    Value
            deriving (Show)

type ObjectMap = M.Map String (WithLocation Object)
