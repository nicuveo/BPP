module Object where



-- imports

import qualified Data.Map as M

import           Grammar
import           Types



-- object

data Object = FunctionObject Function
            | ValueObject    Value
            deriving (Show)

type ObjectMap = M.Map String (WithPos Object)
