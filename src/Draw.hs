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
drawWorld :: World -> Picture
drawWorld w = do let board = board world
                then do let go = show turn world
                    then drawPicture world board turn

-- http://andrew.gibiansky.com/blog/haskell/haskell-gloss/

drawPicture :: World -> Board -> String -> Picture
drawPicture world board turnstr

drawNextPlayer :: String -> Picture

drawTargetReminder :: Int -> Picture

drawGrid :: Picture

drawPieces :: [(Position, Colour)] -> Picture

drawTitle :: Picture