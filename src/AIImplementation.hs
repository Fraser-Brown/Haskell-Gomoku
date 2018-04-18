module AIImplementation where

import Board
import System.Random
import Debug.Trace

-- checks if a given move is valid for a given list of pieces                                                           
checkValid :: [(Position, Col)] -> Position -> Bool
checkValid [] _ = True
checkValid (a:inp) pos | v == pos = False
                       | otherwise = checkValid (inp) pos  
                       where v = fst(a)


-- find position-max score tuple pair in a list with largest max or current eval score and return the position from that tuple
findLargestScore ::  [(Position, Int)] -> (Position, Int) -> Position
findLargestScore [] x = fst x
findLargestScore (x : inp) y | snd x > snd y = findLargestScore inp x
                             | otherwise = findLargestScore inp y

-- findLargestScore [] x = traceStack("Final largest is: ((" ++ show (fst(fst(x))) ++ ", " ++ show (snd(fst(x))) ++ "), " ++ show (snd(x)) ++ ").") fst x
-- findLargestScore (x : inp) y | snd x > snd y = traceStack("Largest now: ((" ++ show (fst(fst(x))) ++ ", " ++ show (snd(fst(x))) ++ "), " ++ show (snd(x)) ++ ").") findLargestScore inp x
--                              | otherwise = traceStack("Largest not: ((" ++ show (fst(fst(x))) ++ ", " ++ show (snd(fst(x))) ++ "), " ++ show (snd(x)) ++ ").") findLargestScore inp y

getMaxEvalScoresFromPoses:: [Position] -> Col -> Board -> Int -> Int -> [(Position, Int)]
getMaxEvalScoresFromPoses poses col startBoard maxDepth nBest = [(pos, findMaxEvalScore pos) | pos <- poses]
                                                                where findMaxEvalScore posIn = getMaxEvalScoreForMove (makePosBoard posIn) 1 maxDepth col nBest
                                                                      makePosBoard posIn = makeBoardWithMove posIn col startBoard

-- Given a list of poses and a starting board and coolour, return a list of tuples of each pos and its immediate eval score if it was to be made (not max recursive)
getCurrentScoresFromPoses:: [Position] -> Board -> Col -> [(Position, Int)]
getCurrentScoresFromPoses poses startBoard col = [(pos, (currentPosScore pos)) | pos <- poses]
                                                 where currentPosScore posIn = evaluate (boardFromPos posIn) col
                                                       boardFromPos posIn = makeBoardWithMove posIn col startBoard


-- get the n best positions (by immediate eval score) for the current board and a given colour
getNBestCurrentPoses :: [(Position, Int)] -> Int -> Int -> [Position]
getNBestCurrentPoses scores index nBest | null scores = []
                                        | index == nBest - 1 = [currentBest]
                                        | otherwise = currentBest : rest
                                          where currentBest = findLargestScore (tail scores) (head scores) :: Position
                                                rest = getNBestCurrentPoses (tail scores) (index + 1) nBest :: [Position]


-- for a given move, recusrively find its children at a given depth (num of moves) limit
-- return the maximum eval score of its children
getMaxEvalScoreForMove :: Board -> Int -> Int -> Col -> Int -> Int
getMaxEvalScoreForMove startBoard depth maxDepth col nBest | winner /= Nothing && getCol(winner) == col = max
                                                           | winner /= Nothing && getCol(winner) == (other col) = - (max)
                                                           | depth == maxDepth = evaluate startBoard col
                                                           | otherwise = recursiveResult -- traceStack("\n\n\n\n---------getMaxEvalScoreForMove---------------\n\n\n") traceStack("recursiveChildResults length = " ++ show ( length recursiveChildResults)) traceStack("childBoards length = " ++ show ( length childBoards)) traceStack("nBestPoses length = " ++ show ( length nBestPoses)) traceStack("posesAndCurrentScores length = " ++ show ( length posesAndCurrentScores)) traceStack("\n\n\n\n------------------------\n\n\n") recursiveResult
                                                           where recursiveResult = maximum recursiveChildResults
                                                                 recursiveChildResults = [getMaxEvalScoreForMove childBoard (depth + 1) maxDepth col nBest | childBoard <- childBoards]
                                                                 childBoards = [makeBoardWithMove pos col startBoard | pos <- nBestPoses]
                                                                 nBestPoses = getNBestCurrentPoses posesAndCurrentScores 0 nBest
                                                                 posesAndCurrentScores = getCurrentScoresFromPoses filteredPoses startBoard col
                                                                 filteredPoses = filterPosesWithAdjacentPieces allPoses (pieces startBoard)
                                                                 allPoses = getAllPoses (pieces startBoard) col ((size startBoard) - 1) ((size startBoard) - 1) (size startBoard)
                                                                 max = maxBound::Int
                                                                 winner = checkWon(startBoard)
                                                                 


-- generate the list of all possible valid moves on a given board (and for a given colour)
getAllPoses:: [(Position, Col)] -> Col -> Int -> Int -> Int -> [Position]
getAllPoses pieces turnCol x y size | x == 0 && y == 0 = []
                                    | checkValid pieces (x,y) = (x,y) : rest
                                    | otherwise = rest -- rest of possible moves on the board which aren't already taken (are valid)
                                    where rest = getAllPoses pieces turnCol newX newY size
                                          newVals = makeNewVals size x y
                                          newY = fst newVals
                                          newX = snd newVals


-- filter list of possible moves
-- only include those for poses next to a piece in any direction
filterPosesWithAdjacentPieces:: [Position] -> [(Position, Col)] -> [Position]
filterPosesWithAdjacentPieces posesIn pieces | null posesIn = []
                                             | firstPosHasAdjacentPieces = firstPos : rest
                                             | otherwise = rest
                                                 where firstPos = head posesIn
                                                       firstPosHasAdjacentPieces = checkIfPosHasAdjacentPieces firstPos pieces
                                                       rest = filterPosesWithAdjacentPieces (tail posesIn) pieces


-- checks if a position (for a move) has >= 1 piece/s next to it
checkIfPosHasAdjacentPieces:: Position -> [(Position, Col)] -> Bool
checkIfPosHasAdjacentPieces pos pieces | null pieces = False
                                       | diffX < 2 && diffY < 2 = True
                                       | otherwise = checkIfPosHasAdjacentPieces pos (tail pieces)
                                       where firstPiecePos = fst (head pieces)
                                             diffX = abs((fst firstPiecePos) - (fst pos))
                                             diffY = abs((snd firstPiecePos) - (snd pos))


-- makes the new pair of coordiante values when recursing in getAllPoses()
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
                                      poses = getAllPoses (pieces board) col (size board - 1) (size board - 1) (size board)
