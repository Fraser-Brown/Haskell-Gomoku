-- contains the overall control of the game and how graphics, event handlers etc. are initialised

module Main where

import Graphics.Gloss
import System.Environment
import System.Directory

import Board
import Draw
import Input
import MinimaxAI

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
         play (InWindow "Gomoku" (1000, 1000) (10, 10)) yellow 10
               (makeWorld(args)) -- in Board.hs
               drawWorld -- in Draw.hs
               handleInput -- in Input.hs
               updateWorld -- in MinimaxAI.hs

makeWorld :: [String] -> World
makeWorld args | length args == 1 = fileReader(args!!0)
               | otherwise = World board Black 100 False
                 where board = initBoard args 
                 
                 
initBoard :: [String] -> Board
initBoard args | length args == 2  = Board (a) (b) []
               | otherwise = Board 6 3 []
               where a = read(args !! 0) :: Int 
                     b = read(args !! 1) :: Int

fileReader:: String -> World 
fileReader file = do exists <- doesFileExist file
                     if exists then
                        do contents <- readFile(file)
                           createFromFile (lines contents) 
                     else do putStrLine "Cannot find file"
                             World (Board 6 3 []) Black 100 False                         

createFromFile :: [String] -> World
createFromFile [] = putStrLine("The file was Empty") Board 6 3 []

createFromFile inp = World x y 100 False
                   where x = Board (findSize inp) (findTarget inp) (findPieces inp [])
                         y = findPlayer inp           


findTarget :: [String] -> Int
findTarget (x:inp) | take 2 x == "TG" = read(filter(\y -> y.isDigit) x) :: Int
                   | length inp > 0 = findTarget(inp)     
                   | otherwise = 3

findSize :: [String] -> Int
findSize (x:inp) | take 2 x == "SZ" = read(filter(\y -> y.isDigit) x) :: Int
                 | length inp > 0 = findTarget(inp)     
                 | otherwise = 6

findPlayer :: [String] -> Col 
findSize (x:inp) | take 2 x == "PL" = if (x !! 3) == "B" then Black
                                                         else White   
                 | length inp > 0 = findTarget(inp)     
                 | otherwise = 6

findPieces :: [Strins] -> [(Position, Col)] -> [(Position, Col)] 
findPieces [] r = r
findPieces (x:inp) pos | take 2 x == "AW" = findPieces inp (pos ++ [(y), White]) 
                       | take 2 x == "AB" = findPieces inp (pos ++ [(y), Black])  
                       | length inp > 0 = findTarget(inp)     
                       | otherwise = []
                       where y = (a,b)
                             jn = drop 3 x --to get just the positions 
                             a = findA(jn, "")   
                             b = findB(jn)

findA:: String-> String -> Int 
findA (x:inp) str | x == ',' = read(str) ::Int
                  | x.isDigit = findA inp (str ++ x)
                  | otherwise = if null inp then str
                                             else findA inp str  

findB:: String -> String ->Int
findB (x:inp) | x == ',' = read(inp) ::Int
              | otherwise = if null inp then -1
                                        else findB inp 