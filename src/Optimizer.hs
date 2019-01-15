module Optimizer (optimize) where

import           Data.List   (dropWhileEnd, foldl', notElem, permutations)
import qualified Data.Map    as M
import           Text.Parsec
import           Text.Printf

optimize :: String -> String
optimize = show . simplify . decompose

data Value = Relative Int
           | Absolute Int
           deriving (Eq, Ord)

combine :: Value -> Value -> Value
combine _            (Absolute x) = Absolute x
combine (Relative x) (Relative d) = Relative $ x + d
combine (Absolute x) (Relative d) = Absolute $ x + d

data Code = Zero
          | Input
          | Output
          | Loop [Code]
          | Assignment Int (M.Map Int Value)
          deriving Eq

instance Show Code where
  showList c = ((c >>= show) ++)
  show Zero             = "[-]"
  show Input            = ","
  show Output           = "."
  show (Loop c)         = printf "[%s]" $ show c
  show (Assignment e a) = res ++ move pos e
    where (res, pos) = foldl' step ("", 0) $ bestOrder $ M.toList $ M.filter (/= Relative 0) a
          step (s, p) (np,c) = (s ++ move p np ++ change c, np)
          move x y
            | y >= x    = replicate (y-x) '>'
            | otherwise = replicate (x-y) '<'
          change (Absolute x) = "[-]" ++ change (Relative x)
          change (Relative x)
            | x >= 0    = replicate   x  '+'
            | otherwise = replicate (-x) '-'
          bestOrder s   = snd $ minimum [(sumDistance p, p) | p <- permutations s]
          sumDistance p = let (d, endP) = foldl' (\(x,cp)(np,_)->(x+abs(np-cp),np)) (0,0) p
                          in d + abs(endP-e)

decompose :: String -> [Code]
decompose = either (error . show) id . parse code "<optimizer>" . filter (`elem` "<>+-.,[]")
  where code = many $ choice [ Input  <$ char ','
                             , Output <$ char '.'
                             , try $ Zero <$ string "[-]"
                             , char '[' *> (Loop <$> code) <* char ']'
                             , interpret <$> many1 (oneOf "<>+-")
                             ]
        interpret s = let (e, a) = foldl' step (0, M.empty) s
                      in  Assignment e $ M.map Relative a
        step (c, m) '>' = (c+1,m)
        step (c, m) '<' = (c-1,m)
        step (c, m) '+' = (c,M.insertWith (+) c   1  m)
        step (c, m) '-' = (c,M.insertWith (+) c (-1) m)
        step _ _ = error "optimizer internal error"

simplify :: [Code] -> [Code]
simplify = clean . dropWhileEnd (`notElem` [Input, Output]) . simplify_
  where simplify_ (Zero           : Zero           : c) = simplify_ $ Zero   : c
        simplify_ (Zero           : Loop _         : c) = simplify_ $ Zero   : c
        simplify_ (Loop l         : Zero           : c) = simplify_ $ Loop l : c
        simplify_ (Loop l         : Loop _         : c) = simplify_ $ Loop l : c
        simplify_ (Loop [Loop l]                   : c) = simplify_ $ Loop l : c
        simplify_ (Loop [Zero]                     : c) = simplify_ $ Zero   : c
        simplify_ (Loop l                          : c) = Loop (simplify_ l) : simplify_ c
        simplify_ (Assignment e a : Zero           : c) = simplify_ $ Assignment e (M.insert e (Absolute 0) a) : c
        simplify_ (Zero           : Assignment e a : c) = simplify_ $ Assignment e (M.insertWith combine 0 (Absolute 0) a) : c
        simplify_ (Assignment e a : Assignment f b : c) = simplify_ $ Assignment (e+f) (M.foldlWithKey' (\m k x -> M.insertWith (flip combine) (e+k) x m) a b) : c
        simplify_ (e:c)                                 = e : simplify_ c
        simplify_ []                                    = []
        clean (Assignment e a : c) = Assignment e (M.map toRelative a) : c
        clean c                    = c
        toRelative (Absolute x) = Relative x
        toRelative v            = v
