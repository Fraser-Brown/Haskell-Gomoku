-- module for the minimax AI adapted from intial code
-- TODO: refine, test and debug the methods below

module AILogic where

import Board
import AIImplementation
import System.Random
import Debug.Trace

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

-- method called to make the AI make a move on the board                       
makeMoveAI:: World -> World                                                              
makeMoveAI world = newWorld 
                     where newWorld = World (newBoard) (other(turn world)) (maxTimer world) (maxTimer world) (False) (typeOfGame world)
                           newBoard = aiNewBoard (world) newPos
                           newPos = chooseMoveMinMax (board world) (turn world)

aiNewBoard:: World -> Position -> Board
aiNewBoard world newPos | checker == Nothing = (board world) 
                        | otherwise = maybeToBoard(checker)
                        where checker = (makeMove (board world) (turn world) newPos)

-- uses minimax to choose the next move to make                           
chooseMoveMinMax :: Board  -> Col -> Position
chooseMoveMinMax board turnCol = findLargestScore nBestPosesAndMaxEvalScores ((-1,-1), 0)
                                    where maxDepth = 5 -- how many moves forward in move tree to look
                                          nBest = 1 -- how many of nodes (ordered by largest to lowest current score) on each tree level to examine recursively
                                          boardSize = size board
                                          maxCoordinate = boardSize - 2
                                          allPoses = getAllPoses (pieces board) turnCol maxCoordinate maxCoordinate boardSize
                                          filteredPoses = filterPosesWithAdjacentPieces allPoses (pieces board)
                                          filteredPosesCurrentScores = getCurrentScoresFromPoses filteredPoses board turnCol
                                          nBestCurrentPoses = getNBestCurrentPoses filteredPosesCurrentScores 1 nBest
                                          nBestPosesAndMaxEvalScores = getMaxEvalScoresFromPoses nBestCurrentPoses turnCol board maxDepth nBest

                                          
maybeToBoard:: Maybe Board -> Board
maybeToBoard (Just x) = x

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