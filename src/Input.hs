-- module to take input events, acting as an API to then call other functions in other modules to process them

module Input(handleInputIO) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AILogic
import Draw

-- Update the world state given an input event
handleInputIO :: Event -> World -> IO World
handleInputIO (EventKey (Char 's') Up _ _) world = saveGame(world)
handleInputIO (EventKey (Char 'c') Up _ _) world = return newWorld 
                                                 where newWorld = World (board world) (other(turn world)) (timer world) (maxTimer world) (paused world) ("PVP")
handleInputIO (EventKey (Char 'a') Up _ _) world = return newWorld 
                                                 where newWorld = World (board world) (other(turn world)) (timer world) (maxTimer world) (paused world) ("AI")                                                 
handleInputIO event world |(typeOfGame world) == "BLANK" = return world
                          |otherwise = return $ handleInput event world
 

saveGame:: World -> IO World
saveGame world = do writeFile("SaveME.txt") (convertWorld(world))
                    return world

convertWorld:: World -> String
convertWorld world = "TG "++ show (target (board world)) ++"\n" ++
                     "SZ "++ show (size (board world)) ++"\n" ++  
                     "TL "++ show (maxTimer world) ++"\n" ++
                     "PL "++ (player) ++"\n" ++ allPieces
                     where allPieces = printPieces(pieces(board world)) ("")
                           player = if (turn world) == Black then "B"
                                                             else "W"  
                                                             
printPieces:: [(Position, Col)] -> String -> String
printPieces [] str = str
printPieces (x:inp) str | snd x == Black = printPieces inp b
                        | otherwise = printPieces inp w
                        where b = str ++ "AB " ++ show(fst(fst x)) ++ "," ++ show(snd(fst x)) ++ "\n"  
                              w = str ++ "AW " ++ show(fst(fst x)) ++ "," ++ show(snd(fst x)) ++ "\n"       

handleInput :: Event -> World -> World

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b = newWorld
                                                                where pos = getPos x y (size (board b))
                                                                      ans = makeMove (board b) (turn b) pos 
                                                                      newWorld = if nothingChecker(ans) == False then  b
                                                                                                                 else World (maybeToBoard(ans)) (other(turn b)) (maxTimer b) (maxTimer b) (False) (typeOfGame b)


handleInput (EventKey (MouseButton RightButton) Up _ _) world = newWorld 
                                                 where newWorld = World (Board (size b) (target b) (p)) (other(turn world)) (maxTimer world) (maxTimer world) (False) (typeOfGame world)
                                                       b = (board world) 
                                                       p = prevBoard (pieces b)
                                                                                                                                                                                                    
handleInput (EventKey (Char 'p') Up _ _) world = newWorld 
                                                 where newWorld = World (board world) (other(turn world)) (timer world) (maxTimer world) (pause) (typeOfGame world)
                                                       pause = if (paused world) then False
                                                                                 else True   
                                                                                                                                      

handleInput(EventKey (SpecialKey KeyUp) Up _ _) world = newWorld 
                                                      where newWorld = World (Board ((size b)+1) (target b) (pieces b)) (turn world) (timer world) (maxTimer world) (paused world) (typeOfGame world)
                                                            b = board world


handleInput(EventKey (SpecialKey KeyDown) Up _ _) world = newWorld 
                                                      where newWorld = World newBoard (turn world) (timer world) (maxTimer world) (paused world) (typeOfGame world)
                                                            b = board world
                                                            newBoard = Board (newSize) (target b) (newPieces)
                                                            newSize = if (size b) == 1 then 1
                                                                                       else (size b) - 1 
                                                            newPieces = removeOverflow (pieces b) [] newSize                            
                                                                                       
handleInput(EventKey (SpecialKey KeyRight) Up _ _) world = newWorld 
                                                      where newWorld = World (Board (size b) ((target b) + 1) (pieces b)) (turn world) (timer world) (maxTimer world) (paused world) (typeOfGame world)
                                                            b = board world


handleInput(EventKey (SpecialKey KeyLeft) Up _ _) world = newWorld 
                                                      where newWorld = World (Board (size b) (newTarget) (pieces b)) (turn world) (timer world) (maxTimer world) (paused world) (typeOfGame world)
                                                            b = board world
                                                            newTarget = if (target b) == 1 then 1
                                                                                       else (target b) - 1                                                                                          
 
                                                                                                                                             
handleInput e b = b

removeOverflow:: [(Position, Col)]-> [(Position, Col)] -> Int -> [(Position, Col)]
removeOverflow [] pieces _ = pieces
removeOverflow (x:inp) pieces s | fst pos >= s = removeOverflow inp pieces s 
                                | snd pos >= s = removeOverflow inp pieces s
                                | otherwise = removeOverflow inp (pieces ++ [x]) s   
                                where pos = fst x    
                  
prevBoard:: [(Position, Col)] ->  [(Position, Col)]
prevBoard [] = []
prevBoard p = init(p)

nothingChecker:: Maybe Board -> Bool
nothingChecker Nothing = False
nothingChecker (Just x) = True

getPos:: Float -> Float -> Int -> Position
getPos x y size = (p, q)
             where p = floor((x+250) / (500/fromIntegral(size)))
                   q = floor((y+250) / (500/fromIntegral(size)))

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}
