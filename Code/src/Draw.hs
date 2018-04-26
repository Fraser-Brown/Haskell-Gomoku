-- module with function for drawing the board to Tereminal

module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Given a world state, return a Picture which will render the world state. - https://hackage.haskell.org/package/gloss-1.1.0.0/docs/Graphics-Gloss-Picture.html
-- Currently just draws a single blue circle as a placeholder.
--
-- World { board :: Board, turn :: Col }
--
-- data Board = Board { size :: Int, target :: Int, pieces :: [(Position, Col)]}
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.

drawWorld :: World -> IO Picture
drawWorld w = return $ gamePicture w            

gamePicture :: World -> Picture
-- get composite parts to arrange combined Picture to display
gamePicture world | (typeOfGame world) == "BLANK" = menu
                  | otherwise = game    
                    where game = Pictures [drawNextPlayer (show(turn world)), 
                                          drawGrid (size (board world)),
                                          drawPieces(pieces (board world)) (size (board world)), 
                                          drawTitle,
                                          drawWinner (board world),
                                          drawTarget (board world),
                                          drawSize (board world),
                                          drawTimer(world)]

                          menu = drawMenu
                          
drawMenu:: Picture
drawMenu = Pictures[Translate (-150) 400 $ Scale  0.6 0.6 $ Text("Gomoku"),
            Translate (-400) 300 $ Scale  0.2 0.2 $ Text("left click : set current players piece"),
            Translate (-400) 250 $ Scale   0.2 0.2 $ Text("right click - undo move "),
            Translate (-400) 200 $ Scale   0.2 0.2 $ Text("p- pause / unpause timer "),
            Translate (-400) 150 $ Scale   0.2 0.2 $ Text("up/down arrow - increase/decrease size of the board "),
            Translate (-400) 100 $ Scale   0.2 0.2 $ Text("right/left - increase/decrease target for game "),
            Translate (-400) 50 $ Scale   0.2 0.2 $ Text("s - save current game at current point to saveME.txt "),
            Translate (-400) 0 $ Scale   0.2 0.2 $ Text("./gomoku filename.txt -> load previous saved game"),
            Translate (-400) (-100) $ Scale   0.4 0.4 $ Text("Press C to play PVP"),
            Translate (-400) (-200) $ Scale   0.4 0.4 $ Text("Press A to play vs AI"),
            Translate (-400) (-300) $ Scale   0.4 0.4 $ Text("Press T to three and three (PVP)"),
            Translate (-400) (-400) $ Scale   0.4 0.4 $ Text("Press F to four and four (PVP)")]

         
                          
drawNextPlayer :: String -> Picture
-- draw text informing the user of which player (Black/White) is next
drawNextPlayer nPlyr = Translate (-150) 300 $ Scale 0.2 0.2 $ Text $ nPlyr ++ " has the next move"

drawGrid :: Int -> Picture
-- draw the lines of the game grid, where n is the grid width and height
drawGrid n = Pictures [verticalLines n, horiLines n]

grey :: Color -- this is the type for Colours in gloss, not the Col type used for pieces/players
grey = makeColor 122 122 122 1

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

drawPieces :: [(Position, Col)] -> Int -> Picture
-- draw each piece on the board
drawPieces pieces size = pictures [drawPiece p size | p <- pieces] 

drawPiece:: (Position, Col) -> Int -> Picture
drawPiece p size = if snd p == White then (Translate x y (color white (circleSolid (fromIntegral(c)) ))) --should be bigger for a smaller board
                                     else (Translate x y (color black (circleSolid (fromIntegral(c)) )))
                                     where pos = fst p
                                           c = round(180/fromIntegral(size))                               
                                           x = fromIntegral((fst pos)) * (500/fromIntegral(size)) - 250 + (500 /fromIntegral(size*2))
                                           y = fromIntegral((snd pos)) * (500/fromIntegral(size)) - 250 + (500 /fromIntegral(size*2))



drawTitle :: Picture
-- draw the title of the game
drawTitle =  Translate (-250) 350 $ Text ("Gomoku")

drawTarget :: Board -> Picture
drawTarget b = Translate (300) 0 $ Scale 0.2 0.2 $ Text $ "Target : " ++ show(target b) 

drawSize :: Board -> Picture
drawSize b = Translate (300) (-100) $ Scale 0.2 0.2 $ Text $ "Size : " ++ show(size b) ++ " * " ++ show(size b) 

drawTimer :: World -> Picture
drawTimer w = Translate (-450) 0 $ Scale 0.2 0.2 $ Text $ "Timer : " ++ show(timer w)
drawWinner:: Board -> Picture
drawWinner board = if winner == Nothing then Blank
                                        else if maybeToCol(winner) == White then Translate (-300) (-400) $ Text ("White Wins")
                                             else Translate (-300) (-400) $  Text("Black Wins")
                  where winner = checkWon(board)  

maybeToCol:: Maybe Col -> Col
maybeToCol (Just x) = x