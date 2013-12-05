{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Types (Walls(..), Cell, Labyrinth, LabyrinthType(..), ProgramOptions(..), emptyCell, fullCell, isFull) where

import Data.Aeson (FromJSON)
import qualified Data.Set as Set
import qualified Data.Vector as V
import GHC.Generics
import System.Random (Random, random, randomR)

{- ========================================================================= -}
{- Labyrinth datatypes                                                       -}
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
{- Utility datatypes                                                         -}
{- ========================================================================= -}

data LabyrinthType = RandomLabyrinth | CellularAutomataLabyrinth
    deriving (Show, Generic)

instance FromJSON LabyrinthType

data ProgramOptions = 
    Options { optIterations      :: Int
            , optBornRule        :: Int
            , optSurviveRule     :: [Int]
            , optLabyrinthWidth  :: Int
            , optLabyrinthHeight :: Int
            , optLabyrinthType   :: LabyrinthType
            }
    deriving (Show, Generic)

instance FromJSON ProgramOptions

{- ========================================================================= -}
{- Functions for helping out with the types                                  -}
{- ========================================================================= -}

isFull :: Cell -> Bool
isFull = (==) fullCell

emptyCell :: Cell
emptyCell = Cell Set.empty

fullCell :: Cell
fullCell = Cell $ Set.fromList [North, East, South, West]

