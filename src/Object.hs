module Object where



-- imports

import qualified Data.Map as M

import           Grammar



-- object

data Object = FunctionObject Function
            | ValueObject    Value
            deriving (Show)

data Entry = Entry { entryFile :: String
                   , entryLine :: Int
                   , entryObj  :: Object
                   }

type ObjectMap = M.Map String Object
