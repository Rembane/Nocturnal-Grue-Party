module Types (LBasic(..), Labyrinth) where

import qualified Data.Vector as V
import System.Random (Random, random, randomR)

{- ========================================================================= -}
{- Some assorted datatypes                                                   -}
{- ========================================================================= -}

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

-- Rows x cols
type Labyrinth = V.Vector (V.Vector LBasic)


