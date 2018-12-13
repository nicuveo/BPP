module Types where



-- imports

import           Text.Printf



-- simple aliases

type Filename  = String
type Raw       = String
type Name      = String



-- position info

data WithPos a = WithPos { posModule :: String
                         , posLine   :: Int
                         , posColumn :: Int
                         , posThing  :: a
                         }

instance Show a => Show (WithPos a) where
  show (WithPos m l c x) = printf "%s:%d:%d:%s" m l c $ show x

instance Functor WithPos where
  fmap f (WithPos m l c x) = WithPos m l c $ f x



-- misc helpers

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f $ g x y
