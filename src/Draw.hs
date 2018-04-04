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
drawWorld w = gamePicture w           

gamePicture :: World -> Picture
-- get composite parts to arrange combined Picture to display
gamePicture world = Pictures [drawNextPlayer (show(turn world)), 
                              --drawTargetReminder(target (board world)),
                              drawGrid (size (board world)),
                              drawPieces(pieces (board world)), 
                              drawTitle,
                              drawWinner (board world)]

drawNextPlayer :: String -> Picture
-- draw text informing the user of which player (Black/White) is next
drawNextPlayer nPlyr = Translate (-150) 350 $ Scale 0.2 0.2 $ Text $ nPlyr ++ " has the next move"

--drawTargetReminder :: Int -> Picture
-- draw text informing user of the target x in a row to get
--drawTargetReminder tgt =  Translate -100 200 $ Scale 0.2 0.2 $ Text $ "Target: " ++ (show tgt) ++ " in a row"

drawGrid :: Int -> Picture
-- draw the lines of the game grid, where n is the grid width and height
drawGrid n = Pictures [verticalLines n, horiLines n]

grey :: Color -- this is the type for colours in gloss, not the Colour type used for pieces/players
grey = makeColor 122 122 122 1

-- TODO: finish the next 2 functions to draw correctly positioned grid lines for the Gomoku board

verticalLines :: Int -> Picture
verticalLines noLines = pictures [drawvertline n noLines | n <- [0..noLines]]
                        
drawvertline :: Int -> Int -> Picture
drawvertline x nolines = color red $ Line [(-250,(c *500) - 250),(250,(c * 500) -250)]
                         where c = (fromIntegral(x)/fromIntegral(nolines))  

horiLines :: Int -> Picture
horiLines noLines = pictures [drawhoriline n noLines | n <- [0..noLines]]
                       
drawhoriline :: Int -> Int -> Picture
drawhoriline x nolines = color red $ Line [((c * 500) -250, -250) ,((c * 500) -250, 250)]
                         where c = (fromIntegral(x)/fromIntegral(nolines)) 

drawPieces :: [(Position, Col)] -> Picture
-- draw each piece on the board
drawPieces pieces = pictures [drawPiece p | p <- pieces] 

drawPiece:: (Position, Col) -> Picture
drawPiece p = if snd p == White then (Translate x y (color white (circleSolid 20)))
                                else (Translate x y (color black (circleSolid 20)))
                                where pos = fst p                               
                                      x = fromIntegral(85  * (fst pos - 2)) - 42.5
                                      y = fromIntegral(85 * (snd pos - 2)) - 42.5



drawTitle :: Picture
-- draw the title of the game
drawTitle =  Translate (-200) 400 $ Text ("Gomoku")

drawWinner:: Board -> Picture
drawWinner board = if winner == Nothing then Blank
                                        else if maybeToCol(winner) == White then Text ("White Wins")
                                             else Text("Black Wins")
                  where winner = checkWon(board)  

maybeToCol:: Maybe Col -> Col
maybeToCol (Just x) = x
--TODO: add Translate method to all methods drawing Picture elements, to correctly position them on the window