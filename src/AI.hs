-- module for the minimax AI adapted from intial code
-- TODO: refine, test and debug the methods below

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

makeMoveAI:: World -> World                                                              
makeMoveAI world = newWorld 
                     where newWorld = World (newBoard) (other(turn world)) (maxTimer world) (maxTimer world) (False) (typeOfGame world)
                           newBoard = maybeTo(makeMove (board world) (turn world) newPos)
                           newPos = chooseMoveMinMax (board world) (turn world)
                           
chooseMoveMinMax :: Board  -> Col -> Position                                    
chooseMoveMinMax board turnCol =    findLargestScore posesAndScores ((-1,-1), 0)
                                          where poses = gen (pieces board) turnCol (size board - 1) (size board - 1) (size board)
                                                posesAndScores = getPosesAndScores poses maxDepth board turnCol
                                                maxDepth = 3

getPosesAndScores :: [Position] -> Int -> Board -> Col -> [(Position, Int)]
getPosesAndScores poses maxDepth board col =  [(pos, scoreFromPos pos) | pos <- poses]
                                                where scoreFromPos posIn = getMaxEvalScoreForMove (makeBoardWithMove posIn col board) 1 maxDepth col

findLargestScore ::  [(Position, Int)] -> (Position, Int) -> Position    
findLargestScore [] x = fst x
findLargestScore (x : inp) y | snd x > snd y = findLargestScore inp x
                             | otherwise = findLargestScore inp y   
                             
                             
getMaxEvalScoreForMove :: Board-> Int -> Int -> Col -> Int
getMaxEvalScoreForMove startBoard depth maxDepth col | depth == maxDepth && finalEvalVals /= [] = maximum finalEvalVals
                                                     | currentEvalScore < minEvalScoreToExamineChildNodes = currentEvalScore
                                                     | recursiveResults /= [] = maximum recursiveResults  
                                                     | otherwise = maxBound :: Int --Potentially change
                                                       where poses = gen (pieces startBoard) col (size startBoard - 1) (size startBoard - 1) (size startBoard)
                                                             finalEvalVals = [evaluate board col | board <- newBoards]
                                                             recursiveResults = [getMaxEvalScoreForMove board (depth + 1) maxDepth col | board <- newBoards]
                                                             newBoards = [makeBoardWithMove pos col startBoard | pos <- poses]
                                                             currentEvalScore = evaluate startBoard col
                                                             minEvalScoreToExamineChildNodes = 0 -- effectively equal with opposition player

gen:: [(Position, Col)] -> Col -> Int -> Int -> Int -> [Position]
gen pieces turnCol x y size | x == 0 && y == 0 = []
                            | checkValid pieces (x,y) = (x,y) : rest
                            | otherwise = rest -- rest of possible moves on the board which aren't already taken (are valid)
                            where rest = gen pieces turnCol newX newY size
                                  newVals = makeNewX size x y
                                  newY = fst newVals
                                  newX = snd newVals

makeNewX:: Int -> Int -> Int -> (Int, Int)
makeNewX size x y | y == 0 && x > 0 = (size - 1, x -1)
                  | y == 0 = (y, 0)
                  | otherwise = (y -1, x)     
                  
makeBoardWithMove:: Position -> Col -> Board -> Board
makeBoardWithMove movePos turnCol startBoard = newBoard
                                               where piecesInWithFriendlyMove = (pieces startBoard) ++ [(movePos, turnCol)]
                                                     enemyMove = minimaxOneMoveDepth (Board (size startBoard) (target startBoard) piecesInWithFriendlyMove) (other turnCol) :: (Position, Col)
                                                     newPieces = piecesInWithFriendlyMove ++ [enemyMove]
                                                     newBoard = Board (size startBoard) (target startBoard) newPieces


-- minimax AI looking only 1 move forward
minimaxOneMoveDepth:: Board -> Col -> (Position, Col)
minimaxOneMoveDepth board col = ((findLargestScore posesAndScores ((-1,-1), 0)), col)
                                where posesAndScores = [(pos, scoreFromPos pos) | pos <- poses]
                                      scoreFromPos posIn = evaluate (makeBoardWithMove posIn col board) col
                                      poses = gen (pieces board) col (size board - 1) (size board - 1) (size board)

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