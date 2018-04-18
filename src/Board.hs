module Board where
    import Data.List (sortBy)
    import Data.Function (on)
    

    data Col = Black | White
      deriving (Show, Eq) 
    
    other :: Col -> Col
    other Black = White
    other White = Black
    
    type Position = (Int, Int)
    
    -- A Board is a record containing the board size (a board is a square grid,
    -- n * n), the number of pieces in a row required to win, and a list 
    -- of pairs of position and the Col at that position.  So a 10x10 board 
    -- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
    -- would be represented as:
    --
    -- Board 10 5 [((5, 5), Black), ((8,7), White)]
    
    data Board = Board { size :: Int,
                         target :: Int,
                         pieces :: [(Position, Col)]
                       }
      deriving (Show,Eq)
      
    initBoard :: [String] -> Board
    initBoard args | length args == 2  = Board (a) (b) []
                   | otherwise = Board 6 3 []
                   where a = read(args !! 0) :: Int 
                         b = read(args !! 1) :: Int

    -- TODO: fix general bugginess of AI v player games
    -- TODO: ensure that moves (world updates, timers, etc) are paused (with AI) when a winning move is made and winning/losing screen is shown, when the AI is deciding on a move, and when the game is over (inc. when all grid squares filled)
    -- TODO: make AI go first (before player) by default


    -- Default board is 6x6, target is 3 in a row, no initial pieces
   
    -- Overall state is the board and whose turn it is, plus any further
    -- information about the world (this may later include, for example, player
    -- names, timers, information about rule variants, etc)
    --
    -- Feel free to extend this, and 'Board' above with anything you think
    -- will be useful (information for the AI, for example, such as where the
    -- most recent moves were).
    data World = World { board :: Board,
                         turn :: Col,
                         timer :: Int,
                         maxTimer:: Int,
                         paused :: Bool,
                         typeOfGame :: String }
    
    -- Play a move on the board; return 'Nothing' if the move is invalid
    -- (e.g. outside the range of the board, or there is a piece already there)
    makeMove :: Board -> Col -> Position -> Maybe Board
    makeMove  board col pos = if (0 <= fst pos && fst pos < size board) && (0 <= snd pos && snd pos < size board) && (isEmpty (pieces board) pos)
                                then Just (Board (size board) (target board) ((pieces board) ++ [(pos, col)]))
                                else Nothing

  --Need to add in the check won to output if the game has been won

    isEmpty :: [(Position, Col)] -> Position -> Bool
    isEmpty [] pos = True

    isEmpty pieces pos | fst x  == pos = False
                       | otherwise = isEmpty (drop 1 pieces) pos
                       where x = head pieces 
    
    -- Check whether the board is in a winning state for either player.
    -- Returns 'Nothing' if neither player has won yet
    -- Returns 'Just c' if the player 'c' has won
        
    targetInARow :: [(Position, Col)] -> Int -> Maybe Col  
    targetInARow pieces target | tooManyCheck w target = Just(White)
                               | tooManyCheck b target = Just(Black)
                               | otherwise = Nothing
                               where w = (filter(\y -> snd y == White) pieces)
                                     b = (filter(\y -> snd y == Black) pieces)


    tooManyCheck:: [(Position, Col)]-> Int-> Bool
    tooManyCheck pieces target | length pieces < target = False
                               | length pieces == target = distanceCheckAll pieces
                               | distanceCheckAll (take target pieces) =  distanceChecker (pieces !! (target -1)) (pieces !! target) == False 
                               | otherwise = tooManyCheck (drop 1 pieces) target
                        
    distanceCheckAll::[(Position, Col)] -> Bool
    distanceCheckAll [] = False
    distanceCheckAll pieces | length pieces == 1 = True
                            | distanceChecker (head pieces) (pieces !! 1) = distanceCheckAll (drop 1 pieces)
                            | otherwise = False
    
    distanceChecker:: (Position, Col) -> (Position, Col)-> Bool    
    distanceChecker a b | abs (x-y) > 1 = False
                        | abs (p-q) > 1 = False
                        | otherwise = True
                        where x = fst(fst a)
                              y = fst(fst b)
                              p = snd(fst a)
                              q = snd(fst b)
    
    checkWon :: Board -> Maybe Col
    checkWon board | r /= Nothing = r
                   | c /= Nothing = c
                   | d1 /= Nothing = d1
                   | d2 /= Nothing = d2
                   | otherwise = Nothing
                   where  r = checkRow board 0
                          c = checkCol board 0
                          d1 = checkDiagonalCriss board 0
                          d2 = checkDiagonalCross board 0

    
    rowScanner ::  [(Position, Col)] -> Int  -> Maybe Col
    rowScanner pieces target | length pieces < target = Nothing
                             | otherwise = targetInARow sorted target
                             where sorted = sortBy (compare `on` (\(a,b) -> snd a)) pieces       

    checkRow :: Board -> Int -> Maybe Col
    checkRow board x | x > s = Nothing
                     | winner /= Nothing = winner
                     | winner == Nothing = checkRow board (x+1) 
                     where p = pieces board
                           s = size board  
                           winner = rowScanner (filter(\y -> fst(fst y) == x) p) (target board)

    colScanner ::  [(Position, Col)] -> Int  -> Maybe Col  
    colScanner pieces target | length pieces < target = Nothing
                             | otherwise = targetInARow sorted target 
                             where sorted = sortBy (compare `on` (\(a,b) -> fst a)) pieces

    checkCol :: Board -> Int -> Maybe Col
    checkCol board x | x > s = Nothing
                     | winner /= Nothing = winner
                     | winner == Nothing = checkCol board (x+1) 
                     where p = pieces board
                           s = size board  
                           winner = colScanner (filter(\y -> snd(fst y) == x) p) (target board)

    diagonalScanner ::  [(Position, Col)] -> Int  -> Maybe Col  
    diagonalScanner pieces target | length pieces < target = Nothing
                                  | otherwise =  targetInARow sorted target
                                  where sorted = sortBy (compare `on` (\(a,b) -> (fst a)) )pieces

    checkDiagonalCriss :: Board -> Int -> Maybe Col --bottom left to top right /
    checkDiagonalCriss board x | x > s * 2 = Nothing
                               | winner /= Nothing = winner
                               | winner == Nothing = checkDiagonalCriss board (x+1) 
                               where p = pieces board
                                     s = size board  
                                     winner = diagonalScanner (filter( \y -> snd(fst y) + fst(fst y) == x) p) (target board)

    checkDiagonalCross :: Board -> Int -> Maybe Col --top left to bottom right \
    checkDiagonalCross board x | x > s * 2 = Nothing
                               | winner /= Nothing = winner
                               | winner == Nothing = checkDiagonalCross board (x+1) 
                               where p = pieces board
                                     s = size board  
                                     winner = diagonalScanner (filter( \y ->  snd(fst y) - fst(fst y) == x - s) p) (target board)
   
    
    evaluate:: Board -> Col -> Int
    evaluate board col | winner /= Nothing && getCol(winner) == col = max
                       | winner /= Nothing && getCol(winner) == (other col) = ((-9) * maxOverTen)
                       | winnerSetup /= Nothing && getCol(winnerSetup) == col = (8 * maxOverTen)
                       | winnerSetup /= Nothing && getCol(winnerSetup) == (other col) = ((-7) * maxOverTen)
                       | otherwise = overall
                        where max = maxBound::Int
                              maxOverTen = max `div` 10
                              winner = checkWon(board)
                              winnerSetup = checkWinSetup(board)
                              ownScore = currCombosScore col board
                              enemyScore = currCombosScore (other col) board
                              overall = ownScore - enemyScore
    
    -- finds if a board contains a player who is certain to win (provided an intelligent move is made next) and if so returns that player
    -- e.g. if a combo of length (target length - 1) has been formed with spaces for final pieces on either side to form the winning combo
    checkWinSetup:: Board -> Maybe Col
    checkWinSetup board = Nothing -- TODO: implement this

                                                 
    currCombosScore:: Col -> Board -> Int
    currCombosScore colIn board = c * sum [(findNoCombosOfLength comboLength colIn board) * (2 ^ (comboLength - 1)) | comboLength <- comboLengths]
                                      where comboLengths = [1.. (target board) -1]
                                            c = 2000

    -- return int showing how many times there is a combo of pieces of a specific given length and of the same (given) colour, on a given board
    findNoCombosOfLength:: Int -> Col -> Board -> Int
    findNoCombosOfLength comboLength col board = r + cl + ci + co
                                                 where r = rowIncrementer board col 0 comboLength 0
                                                       cl = collumnIncrementer board col 0 comboLength 0      
                                                       ci = crissIncrementer board col 0 comboLength 0   
                                                       co = crossIncrementer board col 0 comboLength 0                                      

    
    rowIncrementer :: Board -> Col -> Int -> Int -> Int -> Int
    rowIncrementer board col x len total | x > s = total
                                         | otherwise = rowIncrementer board col (x+1) len (total + c) 
                                         where p = pieces board
                                               s = size board  
                                               c = counter(filter(\y -> fst(fst y) == x && snd(y) == col) p) len  

    collumnIncrementer :: Board -> Col -> Int -> Int -> Int -> Int
    collumnIncrementer board col x len total | x > s = total
                                         | otherwise = collumnIncrementer board col (x+1) len (total + c) 
                                          where p = pieces board
                                                s = size board  
                                                c = counter (filter(\y -> snd(fst y) == x && snd(y) == col) p) len   
    crissIncrementer :: Board -> Col -> Int -> Int -> Int -> Int
    crissIncrementer board col x len total | x > s = total
                                       | otherwise = crissIncrementer board col (x+1) len (total + c) 
                                          where p = pieces board
                                                s = size board  
                                                c = counter (filter(\y -> snd(fst y) + fst(fst y) == x && snd(y) == col) p) len 
                                                
    crossIncrementer :: Board -> Col -> Int -> Int -> Int -> Int
    crossIncrementer board col x len total | x > s = total
                                       | otherwise = crossIncrementer board col (x+1) len (total + c) 
                                          where p = pieces board
                                                s = size board  
                                                c = counter (filter(\y ->  snd(fst y) - fst(fst y) == x - s && snd(y) == col) p) len                                               
    counter ::  [(Position, Col)] -> Int  -> Int
    counter pieces target = length pieces
                              
      
    getCol:: Maybe Col -> Col
    getCol (Just x) = x
    
    