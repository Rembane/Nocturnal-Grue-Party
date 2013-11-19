module Generator where

import Control.Monad (join)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (intersperse)
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, newStdGen, randoms)
import Types

{- ========================================================================= -}
{- Utility functions                                                         -}
{- ========================================================================= -}

chunker' :: Int -> ([a], [a]) -> [[a]]
chunker' n (as, bs) = (as:(chunker' n (splitAt n bs)))

chunker :: Int -> [a] -> [[a]]
chunker n as = chunker' n $ splitAt n as

labyrinthToStr :: Labyrinth -> String
labyrinthToStr lab = concat $ intersperse "\n" $ map (concatMap show) $ V.toList $ V.map V.toList lab

{- ========================================================================= -}
{- The total randomness approach                                             -}
{- ========================================================================= -}

randomLabyrinth :: StdGen -> Int -> Int -> Labyrinth
randomLabyrinth rg height width = V.fromList $ map (V.fromList) $ take height $ chunker width $ randoms rg

{- ========================================================================= -}
{- The cellular automata approach                                            -}
{- ========================================================================= -}

-- Determine if a cell will be born, survive or die.
-- True = It's alive!
-- TODO: Use a set for survive
survival :: (Int, [Int]) -> Int -> Int -> Labyrinth -> Bool
survival (born, survive) y x labyrinth = survival' $ length $ filter (== Wall) $ catMaybes $ map (\(dy, dx) -> cell (dy + y) (dx + x) labyrinth) surroundings
    where
        survival' n    = (born == n) || (fromMaybe False $ fmap (== Wall) (cell y x labyrinth)) && (n `elem` survive)
        surroundings   = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)] -- 9x9 (0, 0), 
        row y' lab     = lab V.!? y'
        cell y' x' lab = join $ fmap (row x') (row y' lab)

boolToCell :: Bool -> LBasic
boolToCell False = Empty
boolToCell True  = Wall

-- Pass it a labyrinth, some rules and you get another one back. :D
maze :: (Int, [Int]) -> Labyrinth -> Labyrinth
maze (born, survive) labyrinth = V.fromList $ map (\y -> V.fromList $ map (\x -> boolToCell $ survival (born, survive) y x labyrinth) [0..height-1]) [0..width-1]
    where 
        height = V.length labyrinth
        width  = V.length $ labyrinth V.! 0

mazeLoop :: Int -> (Labyrinth -> Labyrinth) -> Labyrinth -> Labyrinth
mazeLoop 0 _ lab = lab
mazeLoop n f lab = mazeLoop (n-1) f (f lab)

{- ========================================================================= -}

initialize :: IO StdGen
initialize = do
    newStdGen
    rg <- getStdGen
    return rg

main = do
    args <- getArgs
    rg   <- initialize
    let iterations = read $ head args
    putStrLn $ labyrinthToStr $ mazeLoop iterations (maze (3, [1,2,3,4,5])) (randomLabyrinth rg 20 20)
