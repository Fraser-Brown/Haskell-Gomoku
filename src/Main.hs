-- contains the overall control of the game and how graphics, event handlers etc. are initialised

module Main where

import Graphics.Gloss
import System.Environment

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
makeWorld args = World board Black 100 False
                 where board = initBoard args 
                 
                 
initBoard :: [String] -> Board
initBoard args = if length args < 2 then Board 6 3 []
                                    else Board (a) (b) []
                                         where a = read(args !! 0) :: Int 
                                               b = read(args !! 1) :: Int