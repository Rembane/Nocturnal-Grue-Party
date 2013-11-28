module Types (Walls(..), Cell, Labyrinth, emptyCell, fullCell, isFull) where

import qualified Data.Set as Set
import qualified Data.Vector as V

{- ========================================================================= -}
{- Some assorted datatypes                                                   -}
{- ========================================================================= -}

data Walls = North | East | South | West
    deriving (Eq, Ord, Bounded, Enum, Show)

newtype Cell = Cell (Set.Set Walls)
    deriving (Eq)

instance Show Cell where
    show x | isFull x  = "#"
           | otherwise = "."

-- Rows x cols
type Labyrinth = V.Vector (V.Vector Cell)

{- ========================================================================= -}
{- Functions for helping out with the types                                  -}
{- ========================================================================= -}

isFull :: Cell -> Bool
isFull = (==) fullCell

emptyCell :: Cell
emptyCell = Cell Set.empty

fullCell :: Cell
fullCell = Cell $ Set.fromList [North, East, South, West]

