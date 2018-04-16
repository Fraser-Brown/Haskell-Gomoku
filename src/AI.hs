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
                           newPos = chooseMoveMinMax (board world turn world) 1

-- Generate the list of possible valid moves a given player can make on a Board
gen:: [(Position, Col)] -> Col -> [Position]
gen pieces turnCol x y size | x == 0 && y == 0 = []
                            | checkValid pieces (x,y) = (x,y) : rest
                            | = rest -- rest of possible moves on the board which aren't already taken (are valid)
                            where rest = gen pieces turnCol newX newY size
                                  newY = y - 1
                                  if newY == 0 then if x > 0 then do
                                                                     newY = size - 1
                                                                     newX = x - 1
                                                    else then newX = 0
                                  else then newX = x

-- applies a move from a player to current Board, returning the new resultant Board with the move added
-- TODO: maybe add enemy minimax move here (see next TODO)
makeBoardWithMove:: Position -> Col -> Board -> Board
makeBoardWithMove movePos turnCol startBoard = newBoard
                                               where newBoard = {size board, target board, newPieces}
                                                     newPieces = pieces board ++ (movePos, turnCol)

-- go along maxDepth moves (from 1) and find the max eval score of descendant nodes at the maximum depth for each branch (starting move)
-- recursively pass max eval scores up stack to top call, to then return move with the descendant node with max eval score
-- TODO: find a way to factor in opp player moves (ignores them in makeBoardWithMove presently) - assumes they use same method for max eval score
chooseMoveMinMax :: Board -> Col -> Position
chooseMoveMinMax board turnCol depth = do let moves = gen pieces board turnCol size board - 1 size board - 1 size board
                                              if depth == maxDepth then max [evaluate (makeBoardWithMove pos turnCol board) turnCol | pos <- moves]
                                              else if depth /= 1 then max [chooseMoveMinMax (makeBoardWithMove pos turnCol board) turnCol depth + 1 | pos <- moves]
                                              else then do let scores <- [chooseMoveMinMax (makeBoardWithMove pos turnCol board) turnCol depth + 1 | pos <- moves]
                                                               moves !! (elemIndex max scores)
                                              where maxDepth = 3 -- counted from 1

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