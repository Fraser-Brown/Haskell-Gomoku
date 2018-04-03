-- module to model the board of the game and other elements of the game state
-- TODO: refine, test and debug the methods below

module Board where

data Colour = Black | White -- this is the type for colours of pieces, not the Color type in gloss used to colour Picture elements
  deriving Show

other :: Colour -> Colour -- find the opposite Colour to one given
other Black = White
other White = Black

type Position = (Int, Int) -- x and y coords of a piece on the board

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win (target), and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Colour)]
                   }
  deriving Show

--piecesDoesntContainPos :: [(Position, Colour)] -> Position -> Bool
---- returns if the list of pieces pieces doesn't contain the position given
--piecesDoesntContainPos pieces posIn = not piecesContainsPos pieces posIn

piecesContainsPos :: [(Position, Colour)] -> Position -> Bool
-- returns if the list of pieces pieces contains the position given
piecesContainsPos pieces posIn = posIn `elem` (map fst pieces)

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Colour }

initWorld = World initBoard Black -- function starting the game world with the default initial board, with it being Black's go to start

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Colour -> Position -> Maybe Board

-- given the board to change, and colour and position to put a piece at, put that colour at that pos and return or throw erorr with Nothing if invalid
makeMove board colour pos = if 0 <= fst pos && fst pos < size board && 0 <= snd pos && snd pos < size board -- if a valid input
                                then Just (Board (size board) (target board) (pieces board ++ [(pos, colour)]))
                                else Nothing
--
--
---- Check whether the board is in a winning state for either player.
---- Returns 'Nothing' if neither player has won yet
---- Returns 'Just c' if the player 'c' has won
----checkWon :: Board -> Maybe Colour
--{- for every piece, try going all directions
--    if any have matching colour, keep going in them until target in a row are found -> return that colour or they're empty / diff colour -> next piece
---}
----checkWon board = [if res /= Nothing then res | (pos,clr) <- pieces board, res = checkPiece (pos,clr) board]
----                    then Nothing -- fallback
----                    where checkPiece (pos,clr) board = do let incs = [-1...1], pieces = pieces board, target = target board, size = size board
----                                                          then [if wonInDir \= Nothing then wonInDir | dx <- incs, dy <- incs, wonInDir = checkDir dx dy (pos,clr) pieces target size 0]
----                                                          then Nothing -- fallback
----
----checkDir :: Int -> Int -> (Position, Colour) -> [(Position, Colour)] -> Int -> Int -> Maybe Colour
------ check if there are the no of same coloured pieces in a row in 1 direction from a given start point to win
------ each direction is rerpresented by a dx and dy - showing how much to add/subtract at each jump
----checkDir dx dy (pos,clr) pieces target size incsDone = if not checkCoordsMatching dx dy (pos,clr) pieces (incsDone + 1) then Nothing
----                                                            else if incsDone == target then Just clr
----                                                            else dx dy (pos,clr) pieces target size (incsDone + 1)
----
----checkCoordsMatching :: Int -> Int -> (Position, Colour) -> [(Position, Colour)] -> Int -> Int
------ check the pieces at 2 coords, if present, have the same colour
------ return false if there aren't pieces at both points or if the pieces at both aren't the same colour
----checkCoordsMatching dx dy (pos,clr) pieces jumps = if getColourAtPos pieces (fst pos + jumps * dx, snd pos + jumps * dy) == clr
----
----getColourAtPos :: [(Position, Colour)] -> Int -> Int -> Maybe Colour
------ get the colour of the piece at a given position; if not one there return Nothing
----getColourAtPos pieces x y = [if isPiece then clr | (pos, clr) <- pieces, isPiece = (fst pos == x && snd pos == y)]
----                                then Nothing -- fallback
--

esIgual :: Eq a => a -> a -> Bool
esIgual b c = (b == c)

checkWon :: Board -> Maybe Colour

checkWon board = checkPieces (pieces board) board

checkPieces :: [(Position, Colour)] -> Board -> Maybe Colour
checkPieces [] board = Nothing
checkPieces (x:xs) board = case checkPiece x board of
                                      Just Black -> Just Black
                                      Just White -> Just White
                                      Nothing    -> checkPieces xs board
--
checkPiece :: (Position, Colour) -> Board -> Maybe Colour

checkPiece piece board = do
                      let n = checkNorth piece (pieces board)
                      let ne = checkNorthEast piece (pieces board)
                      let e = checkEast piece (pieces board)
                      let se = checkSouthEast piece (pieces board)
                      let s = checkSouth piece (pieces board)
                      let sw = checkSouthWest piece (pieces board)
                      let w = checkWest piece (pieces board)
                      let nw = checkNorthWest piece (pieces board)
                      if not (esIgual (show n) "Nothing")
                         then n
                      else if not (esIgual (show ne) "Nothing")
                         then ne
                      else if not (esIgual (show e) "Nothing")
                         then e
                      else if not (esIgual (show se) "Nothing")
                         then se
                      else if not (esIgual (show s) "Nothing")
                         then s
                      else if not (esIgual (show sw) "Nothing")
                         then sw
                      else if not (esIgual (show w) "Nothing")
                         then w
                      else if not (esIgual (show nw) "Nothing")
                         then nw
                      else Nothing
--
--
checkNorth :: (Position, Colour) ->[(Position, Colour)]-> Maybe Colour

checkNorth piece pieces  = do
                       let a = show (Just (snd piece))
                       let b = show (returnColourOfPiece (fst (fst piece), snd (fst piece)+1) pieces)
                       let c = show (returnColourOfPiece (fst (fst piece), snd (fst piece)+2) pieces)
                       if esIgual a b && esIgual a c
                           then (Just (snd piece))
                           else Nothing

checkNorthEast :: (Position, Colour) ->[(Position, Colour)]-> Maybe Colour

checkNorthEast piece pieces  = do
                       let a = show (Just (snd piece))
                       let b = show (returnColourOfPiece (fst (fst piece)+1, snd (fst piece)+1) pieces)
                       let c = show (returnColourOfPiece (fst (fst piece)+2, snd (fst piece)+2) pieces)
                       if esIgual a b && esIgual a c
                       then (Just (snd piece))
                       else Nothing

checkEast :: (Position, Colour) ->[(Position, Colour)]-> Maybe Colour

checkEast piece pieces  = do
                       let a = show (Just (snd piece))
                       let b = show (returnColourOfPiece (fst (fst piece)+1, snd (fst piece)) pieces)
                       let c = show (returnColourOfPiece (fst (fst piece)+2, snd (fst piece)) pieces)
                       if esIgual a b && esIgual a c
                       then (Just (snd piece))
                       else Nothing

checkSouthEast :: (Position, Colour) ->[(Position, Colour)]-> Maybe Colour

checkSouthEast piece pieces  = do
                       let a = show (Just (snd piece))
                       let b = show (returnColourOfPiece (fst (fst piece)+1, snd (fst piece)-1) pieces)
                       let c = show (returnColourOfPiece (fst (fst piece)+2, snd (fst piece)-2) pieces)
                       if esIgual a b && esIgual a c
                       then (Just (snd piece))
                       else Nothing

checkSouth :: (Position, Colour) ->[(Position, Colour)]-> Maybe Colour

checkSouth piece pieces  = do
                       let a = show (Just (snd piece))
                       let b = show (returnColourOfPiece (fst (fst piece), snd (fst piece)-1) pieces)
                       let c = show (returnColourOfPiece (fst (fst piece), snd (fst piece)-2) pieces)
                       if esIgual a b && esIgual a c
                       then (Just (snd piece))
                       else Nothing

checkSouthWest :: (Position, Colour) ->[(Position, Colour)]-> Maybe Colour

checkSouthWest piece pieces  = do
                       let a = show (Just (snd piece))
                       let b = show (returnColourOfPiece (fst (fst piece)-1, snd (fst piece)-1) pieces)
                       let c = show (returnColourOfPiece (fst (fst piece)-2, snd (fst piece)-2) pieces)
                       if esIgual a b && esIgual a c
                       then (Just (snd piece))
                       else Nothing

checkWest :: (Position, Colour) ->[(Position, Colour)]-> Maybe Colour

checkWest piece pieces  = do
                       let a = show (Just (snd piece))
                       let b = show (returnColourOfPiece (fst (fst piece)-1, snd (fst piece)) pieces)
                       let c = show (returnColourOfPiece (fst (fst piece)-2, snd (fst piece)) pieces)
                       if esIgual a b && esIgual a c
                       then (Just (snd piece))
                       else Nothing

checkNorthWest :: (Position, Colour) ->[(Position, Colour)]-> Maybe Colour

checkNorthWest piece pieces  = do
                       let a = show (Just (snd piece))
                       let b = show (returnColourOfPiece (fst (fst piece)-1, snd (fst piece)+1) pieces)
                       let c = show (returnColourOfPiece (fst (fst piece)-2, snd (fst piece)+2) pieces)
                       if esIgual a b && esIgual a c
                       then (Just (snd piece))
                       else Nothing

returnColourOfPiece :: Position -> [(Position, Colour)] -> Maybe Colour

returnColourOfPiece position pieces = if piecesContainsPos pieces position == True
                                          then head [Just b | (a,b) <- pieces, (a==position)]
                                          else Nothing


-- The evaluate part is not working yet

{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), Colour) in the 'pieces' list:
- if n == 1, the colour 'Colour' - paramater given - has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}

-- An evaluation function for a minimax search.
-- Given a board and colour of player to score for
-- return an integer indicating how good the board is for that colour.
--evaluate :: Board -> Colour -> Int
--evaluate board colour = do let score = 0
--                                           [score += (2 ** (len - 1)) * noOfCombosOfLength len board colour | len <- [2..5]]
--                                           score
--
--noOfCombosOfLength :: Int -> Board -> Colour -> Int
---- gets the number of combinations of <length> pieces in a row for/of a given Colour
---- find pieces with no others in combo in a downward/left direction and of the given colour
---- for each count the no of peices of same colour in an upward/right direction and return
--noOfCombosOfLength length board colour = do let combos = 0, dirs = [0, 1]
--                                            [if snd piece == colour && matches piece dx dy pieces board length then combos++ | piece <- pieces board, dx <- dirs, dy <- dirs]
--                                            combos
--                                              where matches :: (Position, Colour) -> Int -> Int -> [(Position, Colour)] -> Int -> Bool
--                                                    matches piece dx dy pieces length = do let x = fst fst piece, y = snd fst piece, clr = snd piece
--                                                                                            [if getColourAtPos pieces (x + dx * jumps) (y + dy * jumps) \= clr then False | jumps <- [1..length - 1]]
--                                                                                            True -- fallback
