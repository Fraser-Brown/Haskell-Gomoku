-- module for the minimax AI adapted from intial code
-- TODO: refine, test and debug the methods below

module AI where

import Board
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

-- checks if a given move is valid for a given list of pieces                                                           
checkValid :: [(Position, Col)] -> Position -> Bool
checkValid [] _ = True
checkValid (a:inp) pos | v == pos = False
                       | otherwise = checkValid (inp) pos  
                       where v = fst(a)   

-- method called to make the AI make a move on the board                       
makeMoveAI:: World -> World                                                              
makeMoveAI world = newWorld 
                     where newWorld = World (newBoard) (other(turn world)) (maxTimer world) (maxTimer world) (False) (typeOfGame world)
                           newBoard = maybeTo(makeMove (board world) (turn world) newPos)
                           newPos = chooseMoveMinMax (board world) (turn world)

-- uses minimax to choose the next move to make                           
chooseMoveMinMax :: Board  -> Col -> Position                                    
chooseMoveMinMax board turnCol = traceStack ("\n\n\n\n\nAt start of chooseMoveMinMax() in AI.hs.") findLargestScore posesAndScores ((-1,-1), 0)
                                    where poses = filterPosesWithAdjacentPieces (gen (pieces board) turnCol (size board - 1) (size board - 1) (size board)) (pieces board)
                                          posesAndScores = getPosesAndScores poses maxDepth board turnCol
                                          maxDepth = 5

-- get max possible eval score of child nodes of each move, and return them in tuples with each move
getPosesAndScores :: [Position] -> Int -> Board -> Col -> [(Position, Int)]
getPosesAndScores poses maxDepth board col = traceStack ("At start of getPosesAndScores() in AI.hs.") [(pos, scoreFromPos pos) | pos <- poses]
                                                where scoreFromPos posIn = traceStack ("At start of getMaxEvalScoreForMove() in AI.hs.") getMaxEvalScoreForMove (makeBoardWithMove posIn col board) 1 maxDepth col

-- find position-max score tuple pair in a list with largest max score and return the position
findLargestScore ::  [(Position, Int)] -> (Position, Int) -> Position    
findLargestScore [] x = fst x
findLargestScore (x : inp) y | snd x > snd y = findLargestScore inp x
                             | otherwise = findLargestScore inp y   
                             
-- for a given move, recusrively find its children at a given depth (num of moves) limit
-- return the maximum eval score of its children
getMaxEvalScoreForMove :: Board-> Int -> Int -> Col -> Int
getMaxEvalScoreForMove startBoard depth maxDepth col | checkWon startBoard /= Nothing = if winningCol == col then maxBound :: Int else (- (maxBound)) :: Int
                                                     | depth == maxDepth = traceStack ("At start of getMaxEvalScoreForMove() in AI.hs           at child node.") finalEvalVal
                                                     | currentEvalScore < minEvalScoreToExamineChildNodes = traceStack ("At start of getMaxEvalScoreForMove() in AI.hs           pruning.") currentEvalScore
                                                     | recursiveResults /= [] = traceStack ("At start of getMaxEvalScoreForMove() in AI.hs           recursing at depth" ++ show depth ++ ".") maximum recursiveResults
                                                     -- | traceStack ("At start of makeBoardWithMove() in AI.hs.") otherwise = maxBound :: Int --Potentially change
                                                       where genPoses = traceStack ("Running gen.") gen (pieces startBoard) col (size startBoard - 1) (size startBoard - 1) (size startBoard)
                                                             poses = traceStack ("Filtering poses.") filterPosesWithAdjacentPieces genPoses (pieces startBoard)
                                                             finalEvalVal = evaluate startBoard col
                                                             recursiveResults = [getMaxEvalScoreForMove board (depth + 1) maxDepth col | board <- newBoards]
                                                             newBoards = [makeBoardWithMove pos col startBoard | pos <- poses]
                                                             currentEvalScore = evaluate startBoard col
                                                             minEvalScoreToExamineChildNodes = (target startBoard - 2) * (2 ^ (target startBoard - 2))
                                                             winningCol = getCol (checkWon startBoard)

-- generate the list of all possible valid moves on a given board (and for a given colour)
gen:: [(Position, Col)] -> Col -> Int -> Int -> Int -> [Position]
gen pieces turnCol x y size | x == 0 && y == 0 = []
                            | checkValid pieces (x,y) = (x,y) : rest
                            | otherwise = rest -- rest of possible moves on the board which aren't already taken (are valid)
                            where rest = gen pieces turnCol newX newY size
                                  newVals = makeNewVals size x y
                                  newY = fst newVals
                                  newX = snd newVals

-- filter list of possible moves
-- only include those for poses next to a piece in any direction
filterPosesWithAdjacentPieces:: [Position] -> [(Position, Col)] -> [Position]
filterPosesWithAdjacentPieces posesIn pieces | posesIn == [] = []
                                             | firstPosHasAdjacentPieces = firstPos : rest
                                             | otherwise = rest
                                                 where firstPos = head posesIn
                                                       firstPosHasAdjacentPieces = traceStack ("At start of checkIfPosHasAdjacentPieces() in AI.hs.") checkIfPosHasAdjacentPieces firstPos pieces
                                                       rest = filterPosesWithAdjacentPieces (tail posesIn) pieces

-- checks if a position (for a move) has >= 1 piece/s next to it
checkIfPosHasAdjacentPieces:: Position -> [(Position, Col)] -> Bool
checkIfPosHasAdjacentPieces pos pieces | null pieces = False
                                       | diffX < 2 && diffY < 2 = True
                                       | otherwise = checkIfPosHasAdjacentPieces pos (tail pieces)
                                       where firstPiecePos = fst (head pieces)
                                             diffX = abs((fst firstPiecePos) - (fst pos))
                                             diffY = abs((snd firstPiecePos) - (snd pos))

-- makes the new pair of coordiante values when recursing in gen()
makeNewVals:: Int -> Int -> Int -> (Int, Int)
makeNewVals size x y | y == 0 && x > 0 = (size - 1, x -1)
                  | y == 0 = (y, 0)
                  | otherwise = (y -1, x)     
                  
-- apply a given friendly move to a given starting board
-- then also apply an enemy move to it
-- the enemy move works from a minimax AI looking 1 move forwards                  
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
                                      scoreFromPos posIn = evaluate (Board (size board) (target board) ((pieces board) ++ [(posIn, col)])) col
                                      poses = gen (pieces board) col (size board - 1) (size board - 1) (size board)

--TODO: fix error "gomoku: src/AI.hs:139:1-20: Non-exhaustive patterns in function maybeTo"
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