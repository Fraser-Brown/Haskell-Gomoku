-- contains the overall control of the game and how graphics, event handlers etc. are initialised

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Environment
import System.Directory
import Data.Char

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move




main :: IO ()
main =do args <- getArgs
         world <- readWorld args

         playIO (InWindow "Gomoku" (1200, 1000) (10, 10)) yellow 10
               world
               drawWorld -- in Draw.hs
               handleInputIO -- in Input.hs
               updateWorld -- in AI.hs
         



defaultWorld = World (initBoard []) Black 100 100 False "BLANK"

readWorld :: [String] -> IO World
readWorld [] = return defaultWorld
readWorld args = case args of
                  [] -> return defaultWorld
                  strings -> do exists <- doesFileExist(args!!0)
                                case exists of
                                     False -> return (cmdLineWorld args)
                                     True -> do contents <- readFile(args!!0)
                                                case contents of
                                                   [] -> return defaultWorld
                                                   strings -> return (createFromFile(lines strings))


cmdLineWorld::[String] -> World     
cmdLineWorld args = World b Black 100 100 False "BLANK" --may wish to add full cmd line args and input checking
                  where b = Board s t []
                        s = read(args!!0) :: Int
                        t = read(args!!1) :: Int                                         

createFromFile :: [String] -> World
createFromFile [] = World (initBoard []) Black 100 100 False "BLANK"

createFromFile inp = World (x) (y) (findTimeLimt(inp)) (findTimeLimt(inp)) (False) "BLANK" --may wish to add this on load
                     where x = (Board (findSize inp) (findTarget inp) (findPieces inp []))
                           y = (findPlayer inp)           



---------------------------------------------------------------------------------------------------


findTarget :: [String] -> Int
findTarget (x:inp) | take 2 x == "TG" = read(filter(\y -> isDigit(y)) x) :: Int
                   | length inp > 0 = findTarget(inp)     
                   | otherwise = 4

findSize :: [String] -> Int
findSize (x:inp) | take 2 x == "SZ" = read(filter(\y -> isDigit(y)) x) :: Int
                 | length inp > 0 = findSize(inp)     
                 | otherwise = 7

findTimeLimt :: [String] -> Int
findTimeLimt (x:inp) | take 2 x == "TL" = read(filter(\y -> isDigit(y)) x) :: Int
                     | length inp > 0 = findTimeLimt(inp)     
                     | otherwise = 100                 
--the three above could be condensed into 1 method, left for readability for now


findPlayer :: [String] -> Col 
findPlayer (x:inp) | take 2 x == "PL" = if (x !! 3) == 'B' then Black
                                                           else White   
                   | length inp > 0 = findPlayer(inp)     
                   | otherwise = Black

findPieces :: [String] -> [(Position, Col)] -> [(Position, Col)]  --done this way to preserve move order, so that the undo will still work (potential for learning ai also)
findPieces [] r = r
findPieces (x:inp) pos | take 2 x == "AW" = findPieces inp (pos ++ [((y), White)]) 
                       | take 2 x == "AB" = findPieces inp (pos ++ [((y), Black)])  
                       | length inp > 0 = findPieces (inp) pos    
                       | otherwise = []
                       where y = (a,b)
                             jn = drop 3 x --to get just the positions 
                             a = read(findA jn "") :: Int   
                             b = read(findB(jn)) ::Int

findA:: String -> String -> String 
findA (x:inp) str | x == ',' = str
                  | isDigit(x) = findA inp (str ++ [x])
                  | otherwise = if null inp then str
                                             else findA inp str  

findB:: String -> String
findB (x:inp) | x == ',' = (filter(\y -> isDigit(y)) inp)
              | length inp > 0 = findB inp
              | otherwise = "-1"
                              