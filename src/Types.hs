module Types where



-- imports

import           Text.Printf



-- simple aliases

type Filename  = String
type Raw       = String
type Name      = String



-- position info

data Location = Location { locFileName :: String
                         , locLine     :: Int
                         , locColumn   :: Int
                         }

data WithLocation a = WL { getLocation :: Location
                         , getEntry    :: a
                         }

instance Show a => Show (WithLocation a) where
  show (WL (Location m l c) x) = printf "%s:%d:%d:%s" m l c $ show x

instance Functor WithLocation where
  fmap f (WL l x) = WL l $ f x



-- misc helpers

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f $ g x y
