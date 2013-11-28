module Main where

import Data.Aeson (eitherDecode)
import Data.Either (Either(..))
import qualified Data.ByteString.Lazy as B
import System.Random (StdGen, getStdGen, newStdGen)
import Generator
import Types 

initialize :: IO StdGen
initialize = do
    newStdGen
    rg <- getStdGen
    return rg

getJSON :: IO B.ByteString
getJSON = B.readFile "configuration.json"

main = do
    rg       <- initialize
    settings <- fmap eitherDecode getJSON
    case settings of 
        Left err -> do
            putStrLn err 
        Right d  -> do
            let its     = optIterations d
            let born    = optBornRule d
            let survive = optSurviveRule d
            let height  = optLabyrinthHeight d
            let width   = optLabyrinthWidth d
            putStrLn $ labyrinthToStr $ mazeLoop its (maze (born, survive)) (randomLabyrinth rg height width)

