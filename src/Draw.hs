-- module with function for drawing the board to Tereminal

module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Given a world state, return a Picture which will render the world state. - https://hackage.haskell.org/package/gloss-1.1.0.0/docs/Graphics-Gloss-Picture.html
-- Currently just draws a single blue circle as a placeholder.
--
-- World { board :: Board, turn :: Colour }
--
-- data Board = Board { size :: Int, target :: Int, pieces :: [(Position, Colour)]}
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
--drawWorld :: World -> Picture
drawWorld w = display 
(InWindow
       "Gomoku"     -- window title
        (1000, 700)       -- window size
        (10, 10))        -- window position
white                    -- background color
gamePicture world              -- Picture object to draw, made of composite parts

gamePicture :: World -> Picture
-- get composite parts to arrange combined Picture to display
gamePicture world = Pictures [drawNextPlayer show turn board, drawTargetReminder target board, drawGrid size board, drawPieces pieces board size board, drawTitle]

drawNextPlayer :: String -> Picture
-- draw text informing the user of which player (Black/White) is next
drawNextPlayer nPlyr = $ Text nPlyr ++ " has the next move"

drawTargetReminder :: Int -> Picture
-- draw text informing user of the target x in a row to get
drawTargetReminder tgt = $ Text "Target: " ++ show tgt ++ " in a row"

drawGrid :: Int -> Picture
-- draw the lines of the game grid, where n is the grid width and height
drawGrid n = Pictures [verticalLines n, horiLines n]

grey :: Color -- this is the type for colours in gloss, not the Colour type used for pieces/players
grey = makeColor 122 122 122 1

-- TODO: finish the next 2 functions to draw correctly positioned grid lines for the Gomoku board

verticalLines :: Int -> Picture
verticalLines n = Pictures [drawvertLine no n | no <- [1..n]]
                    where drawvertLine num size = grey Seg

horiLines :: Int -> Picture
horiLines n = Pictures [drawhoriLine no n | no <- [1..n]]
                where drawhoriLine num size = grey Seg

drawPieces :: [(Position, Colour)] -> Picture
-- draw each piece on the board
drawPieces pieces = Pictures [drawPiece p | p <- pieces]
                        where drawPiece p = if snd p == White then white Circle 2
                                                else then black Circle 2

drawTitle :: Picture
-- draw the title of the game
drawTitle = $ Text "Gomoku"

--TODO: add Translate method to all methods drawing Picture elements, to correctly position them on the window