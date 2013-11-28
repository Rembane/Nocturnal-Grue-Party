module Generator where

import Control.Monad (join)
import qualified Data.Set as Set
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

-- In the simplest case, the cell is either surrounded by walls or empty
randomLabyrinth :: StdGen -> Int -> Int -> Labyrinth
randomLabyrinth rg height width = V.fromList $ map (V.fromList) $ take height $ chunker width $ map cellFactory $ randoms rg
    where
        cellFactory False = emptyCell
        cellFactory True  = fullCell

{- ========================================================================= -}
{- The cellular automata approach                                            -}
{- ========================================================================= -}

-- Determine if a cell will be born, survive or die.
-- Returns the new cell type
-- TODO: Use a set for survive
survival :: (Int, [Int]) -> Int -> Int -> Labyrinth -> Cell
survival (born, survive) y x labyrinth = survival' $ length $ filter isFull $ catMaybes $ map (\(dy, dx) -> cell (dy + y) (dx + x) labyrinth) surroundings
    where
        survival' n | born == n  = fullCell                    
                    | birthing n = fullCell 
                    | otherwise  = emptyCell
        surroundings   = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)] -- 3x3 but not (0, 0)
        row y' lab     = lab V.!? y'
        cell y' x' lab = join $ fmap (row x') (row y' lab)
        -- This function determines if a wall will be born or not. Int -> Bool
        birthing n     = (fromMaybe False $ fmap isFull (cell y x labyrinth)) && (n `elem` survive)

-- Pass it a labyrinth, some rules and you get another one back. :D
maze :: (Int, [Int]) -> Labyrinth -> Labyrinth
maze (born, survive) labyrinth = V.fromList $ map (\y -> V.fromList $ map (\x -> survival (born, survive) y x labyrinth) [0..height-1]) [0..width-1]
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
    -- putStrLn $ labyrinthToStr $ (randomLabyrinth rg 20 20)
