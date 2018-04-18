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

-- TODO: fix eval scores always 0 bug
-- TODO: fix getNBestCurrentPoses - doesn't return any

-- uses minimax to choose the next move to make                           
chooseMoveMinMax :: Board  -> Col -> Position
chooseMoveMinMax board turnCol = traceStack("\n\n\n\n------------------------\n\n\n" ++ "nBestCurrentPoses null = " ++ show ( null nBestCurrentPoses))
                                 traceStack("nBestCurrentPoses length = " ++ show ( length nBestCurrentPoses))

                                 traceStack("\n\nBestCurrentPoses 0: (" ++ show (fst (getItemInPoses 0 nBestCurrentPoses)) ++ ", " ++ show (snd (getItemInPoses 0 nBestCurrentPoses)) ++ ").")


                                 traceStack("nBestPosesAndMaxEvalScores null = " ++ show ( null nBestPosesAndMaxEvalScores))
                                 traceStack("nBestPosesAndMaxEvalScores length = " ++ show ( length nBestPosesAndMaxEvalScores))

                                 traceStack("\n\nnBestPosesAndMaxEvalScores 0: ((" ++ show (fst (fst (getItemInPosesAndScores 0 nBestPosesAndMaxEvalScores))) ++ ", " ++ show (snd (fst (getItemInPosesAndScores 0 nBestPosesAndMaxEvalScores))) ++ "), " ++ show (snd (getItemInPosesAndScores 0 nBestPosesAndMaxEvalScores)) ++ ").")

                                 traceStack("Optimal move: (" ++ show (fst optimalMove) ++ ", " ++ show (snd optimalMove) ++ ").")
                                 traceStack(show (snd (head nBestPosesAndMaxEvalScores)) ++ "\n\n\n\n------------------------\n\n\n")


                                 optimalMove
                                    where maxDepth = 5 -- how many moves forward in move tree to look
                                          nBest = 1 -- how many of nodes (ordered by largest to lowest current score) on each tree level to examine recursively
                                          boardSize = size board
                                          maxCoordinate = boardSize - 1
                                          allPoses = getAllPoses (pieces board) turnCol maxCoordinate maxCoordinate boardSize
                                          filteredPoses = filterPosesWithAdjacentPieces allPoses (pieces board)
                                          filteredPosesCurrentScores = getCurrentScoresFromPoses filteredPoses board turnCol
                                          nBestCurrentPoses = getNBestCurrentPoses filteredPosesCurrentScores 0 nBest -- TODO: fix error which makes this empty
                                          nBestPosesAndMaxEvalScores = getMaxEvalScoresFromPoses nBestCurrentPoses turnCol board maxDepth nBest
                                          optimalMove = findLargestScore nBestPosesAndMaxEvalScores ((-1,-1), 0)

                                          
getItemInPoses index list | index == 0 = head list
                          | otherwise = getItemInPoses (index - 1) (tail list)

getItemInPosesAndScores index list | index == 0 = head list
                                   | otherwise = getItemInPoses (index - 1) (tail list)

                                          
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