module Misc where

-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f $ g x y
