module Types where



-- simple aliases

type Filename  = String
type Raw       = String
type Name      = String




-- misc helpers

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f $ g x y
