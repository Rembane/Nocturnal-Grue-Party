module Generator where

import Data.List (intersperse)
import System.Random (Random, StdGen, getStdGen, random, randomR, randoms)

data LBasic = Empty | Wall
    deriving (Eq, Ord, Bounded, Enum)

instance Show LBasic where
    show Empty = "."
    show Wall  = "#"

instance Random LBasic where
    random g = case randomR (fromEnum (minBound :: LBasic), fromEnum (maxBound :: LBasic)) g of
        (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
        (r, g') -> (toEnum r, g')

type LRow      = [LBasic]
type Labyrinth = [LRow]

data Walker a = NotVisited a | Visited a
    deriving (Eq, Show)

type WalkerLabyrinth = [[Walker LBasic]]

fill :: Int -> Int -> LBasic -> Labyrinth
fill height width c = replicate height $ replicate width c

chunker' :: Int -> ([a], [a]) -> [[a]]
chunker' n (as, bs) = (as:(chunker' n (splitAt n bs)))

chunker :: Int -> [a] -> [[a]]
chunker n as = chunker' n $ splitAt n as

{- ========================================================================= -}
{- The total randomness approach                                             -}
{- ========================================================================= -}

randomRow :: StdGen -> Int -> LRow
randomRow rg width = take width $ randoms rg

randomLabyrinth :: StdGen -> Int -> Int -> Labyrinth
randomLabyrinth rg height width = take height $ chunker width $ randoms rg

{- ========================================================================= -}
{- The depth first approach                                                  -}
{- ========================================================================= -}

depthFirstLabyrinth :: StdGen -> Int -> Int -> Int -> Labyrinth
depthFirstLabyrinth rg height width iterations = walker height width rg wLabyrinth iterations 
    where wLabyrinth = map (map NotVisited) $ fill height width Wall

move :: Int -> Int -> WalkerLabyrinth -> WalkerLabyrinth
move newRow newCol wl = map moveRow $ zip wl [0..] 
    where moveRow (r,i) = if i == newRow
                          then map moveCol (zip r [0..])
                          else r
          moveCol (c,i) = if i == newCol
                          then changeCol c
                          else c

          changeCol (NotVisited x) = Visited Empty
          changeCol x              = x

walker :: Int -> Int -> StdGen -> WalkerLabyrinth -> Int -> Labyrinth
walker height width rg iterations wla = stripWalker $ walker' startingRow startingCol (moveList g3) wla iterations 
    where 
        moves             = zip [-1, 0, 0, 1] [0, -1, 1, 0]
        (startingRow, g2) = randomR (0, height) rg
        (startingCol, g3) = randomR (0, width) g2
        moveList gen      = let (idx, gnew) = randomR (0,3) gen
                            in (moves !! idx : moveList gnew)

        stripWalker       = map (map sw)
        sw (Visited x)    = x
        sw (NotVisited x) = x

        walker' _   _   _                       0    wl               = wl
        walker' row col ((diffRow, diffCol):ms) iter wl | outofBounds = walker' row col moves iter wl -- Just skip!
                                                        | otherwise   = walker' newRow newCol moves (iter-1) (move newRow newCol wl)
            where 
                newRow = row + diffRow
                newCol = col + diffCol
                outofBounds = newRow == height || newRow < 0 || newCol == width || newCol < 0

{- ========================================================================= -}

main = do
    rg <- getStdGen
    -- putStrLn $ concat $ intersperse "\n" $ map (concatMap show) $ randomLabyrinth rg 20 20
    putStrLn $ concat $ intersperse "\n" $ map (concatMap show) $ depthFirstLabyrinth rg 10 10 10

    
