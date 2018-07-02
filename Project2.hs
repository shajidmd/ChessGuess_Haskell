--  File     : Project2.hs
--  Author   : Shajid Mohammad (shajidm@student.unimelb.edu.au)
--  Purpose  : Assignment2 project

--  Module declaration
module Project2 (initialGuess, nextGuess, GameState) where

-- 1st Question
import Data.List
import qualified Data.Set as Set


type Pieces = [String] -- ["BK", "WR"]
colors = ["B", "W"]
kinds = ["P", "P", "P", "P", "P", "P", "P", "P", "B", "B", "N", "N", "R", "R", "K", "Q"]

type Feedback = (Int, Int, Int)
-- (p, k, c)


type GameState = [[String]]
-- [(("BK", "BN"), 2)]

initialGuess :: Int -> (Pieces, GameState)
initialGuess size | size <= 6 = (take size ["B" ++ k | k <- nub kinds], iniGs size)
                  | size > 6 && size <= 16 = (take size ["B" ++ k | k <- kinds], iniGs size)
                  | size > 16 =  (take size [c ++ k | c <- colors, k <- kinds], iniGs size)
  
iniGs :: Int -> GameState
iniGs size = [i | i <- (Set.toList (Set.fromList (concat ([combinations s [c ++ k | c <- colors, k <- kinds] | s <- [0..size]]))))]

iniGs2 :: Int -> GameState
iniGs2 size = [i | i <- (Set.toList (Set.fromList (concat ([combinations s ["W" ++ k | k <- kinds] | s <- [0..size]]))))]



nextGuess :: (Pieces,GameState) -> (Int,Int,Int) -> (Pieces,GameState)
nextGuess (demo1,demo2) (a, b, c) | a==0 && c==0 = ((head demo2), iniGs2 b)


response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightKind, rightColor)
  where 
        common      = mintersect guess target
        right       = length common
        rguess      = foldr (delete) guess common
        rtarget     = foldr (delete) target common
        rightColor  = length $ mintersect (map (!!0) rguess) (map (!!0) rtarget)
        rightKind   = length $ mintersect (map (!!1) rguess) (map (!!1) rtarget)

mintersect :: Eq t => [t] -> [t] -> [t]
mintersect [] _ = []
mintersect (x:xs) r = if elem x r then x : mintersect xs (delete x r)
                      else mintersect xs r 


-- Using list comprehensions
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

--nextGuess :: ([String],GameState) → (Int,Int,Int) → ([String],GameState),()


