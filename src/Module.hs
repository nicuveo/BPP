module Module where



-- imports

import           Text.Printf

import           Types



-- location info

data Location = SourceFile { locSource :: Filename
                           , locLine   :: Int
                           , locColumn :: Int
                           }
              | BuiltIn

data WithLocation a = WL { getLocation :: Location
                         , getEntry    :: a
                         }

instance Show a => Show (WithLocation a) where
  show (WL loc x) = case loc of
    BuiltIn          -> printf "<built-in function>: %s" $ show x
    SourceFile s l c -> printf "%s:%d:%d: %s" s l c      $ show x

instance Functor WithLocation where
  fmap f (WL l x) = WL l $ f x
