-- module for the AI adapted from intial code

module AI where

import Board
import System.Random

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }


updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> IO World

updateWorld t w | (typeOfGame w) == "AI" && (turn w) == Black = return (makeMoveAI w)
                | (typeOfGame w) == "AI" = return (timedWorld w)  
                | (typeOfGame w) == "PVP"  = return (timedWorld w)
                | otherwise = return w 


timedWorld :: World -> World
timedWorld w = newWorld
               where newWorld = World (board w) (player) (t) (maxTimer w) (paused w) (typeOfGame w)
                     player = if t == 0 then other(turn w)
                                        else (turn w)
                     t = if paused w then (timer w) 
                                     else if (timer w) > 0 then (timer w) - 1
                                                           else 100 

                                                           
checkValid :: [(Position, Col)] -> Position -> Bool
checkValid [] _ = True
checkValid (a:inp) pos | v == pos = False
                       | otherwise = checkValid (inp) pos  
                       where v = fst(a)


-- use gen method to generate list of every possible valid move
-- then go along tree (of moves) up to depth limit (x moves in future) and check evaluation scores of resultant boards
-- choose move that results in descendants with best eval scores (max, or average, or combo of measures..?)
makeMoveAI:: World -> World                                                              
makeMoveAI world = newWorld 
                     where newWorld = World (newBoard) (other(turn world)) (maxTimer world) (maxTimer world) (False) (typeOfGame world)
                           newBoard = maybeTo(makeMove (board world) (turn world) newPos)
                           newPos = chooseMoveMinMax (board world turn world)

gen:: [(Position, Col)] -> Col -> [Position]
gen pieces turnCol x y size | x == 0 && y == 0 = []
                            | checkValid pieces (x,y) = (x,y) : rest
                            | = rest
                            where rest = gen pieces turnCol newX newY size
                                  newY = y - 1
                                  if newY == 0 then if x > 0 then do
                                                                     newY = size - 1
                                                                     newX = x - 1
                                                    else then newX = 0
                                  else then newX = x

makeBoardWithMove:: Position -> Col -> Board -> Board
makeBoardWithMove movePos turnCol startBoard = newBoard
                                               where newBoard = {size board, target board, newPieces}
                                                     newPieces = pieces board ++ (movePos, turnCol)

chooseMoveMinMax :: Board -> Col -> Position
chooseMoveMinMax board turnCol = do let moves = gen pieces board turnCol size board - 1 size board - 1 size board
                                    do let scores <- [evaluate makeBoardWithMove pos turnCol board | pos <- moves]
                                          moves !! (elemIndex max scores)

maybeTo:: Maybe a -> a
maybeTo (Just x) = x

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either
 player has won and display a message if so.
-}