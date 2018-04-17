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


-- get max possible eval score of child nodes of each move, and return them in tuples with each move
getPosesAndMaxEvalScores :: [Position] -> Int -> Board -> Col -> Int -> [(Position, Int)]
getPosesAndMaxEvalScores poses maxDepth board col nBest = [(pos, scoreFromPos pos) | pos <- poses]
                                                          where scoreFromPos posIn = getMaxEvalScoreForMove (makeBoardWithMove posIn col board) 1 maxDepth col nBest


-- find position-max score tuple pair in a list with largest max or current eval score and return the position from that tuple
findLargestScore ::  [(Position, Int)] -> (Position, Int) -> Position
findLargestScore [] x = fst x
findLargestScore (x : inp) y | snd x > snd y = findLargestScore inp x
                             | otherwise = findLargestScore inp y


-- Given a list of poses and a starting board and coolour, return a list of tuples of each pos and its immediate eval score if it was to be made (not max recursive)
getCurrentScoresFromPoses:: [Position] -> Board -> Col -> [(Position, Int)]
getCurrentScoresFromPoses poses startBoard col = [(pos, (currentPosScore pos)) | pos <- poses]
                                                 where currentPosScore posIn = evaluate (boardFromPos posIn) col
                                                       boardFromPos posIn = makeBoardWithMove posIn col startBoard


-- get the n best positions (by max recursive eval score) for the current board and a given colour
getNBestPoses:: Board -> Col -> Int -> [Position]
getNBestPoses board col nBest | not (null filtered) = nBestFilterered
                              | otherwise = nBestAll
                              where all = getAllPoses (pieces board) col (size board - 1) (size board - 1) (size board)
                                    filtered = filterPosesWithAdjacentPieces all (pieces board)
                                    filteredPosesAndCurrentScores = getCurrentScoresFromPoses filtered
                                    nBestFilterered = getNBestCurrentPoses (filteredPosesAndCurrentScores board col nBest)
                                    nBestAll = 1 getNBestCurrentPoses (allPosesAndCurrentScores board col nBest)


-- get the n best positions (by immediate eval score) for the current board and a given colour
getNBestCurrentPoses :: [(Position, Int)] -> Int -> Int -> [Position]
getNBestCurrentPoses scores index nBest | index == nBest || scores == [] = []
                                        | otherwise = [currentBest] ++ rest
                                          where currentBest = findLargestScore scores
                                                rest = getNBestCurrentPoses (tail scores) (index + 1) nBest


-- for a given move, recusrively find its children at a given depth (num of moves) limit
-- return the maximum eval score of its children
getMaxEvalScoreForMove :: Board -> Int -> Int -> Col -> Int -> Int
getMaxEvalScoreForMove startBoard depth maxDepth col nBest | checkWon startBoard /= Nothing = if getCol (checkWon startBoard) == col then maxBound :: Int else (- (maxBound)) :: Int
                                                           | depth == maxDepth = evaluate startBoard col
                                                           | otherwise = maximum recursiveResults
                                                           where poses = getNBestPoses (startBoard col nBest)
                                                                 posBoards = [makeBoardWithMove (pos col startBoard) | pos <- poses]
                                                                 recursiveResults = [getMaxEvalScoreForMove (board (depth + 1) maxDepth col) | board <- posBoards]


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
