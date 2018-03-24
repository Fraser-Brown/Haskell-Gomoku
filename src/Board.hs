-- module to model the board of the game and other elements of the game state

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
makeMove board colour pos = if 0 <= fst pos < size board && 0 <= snd pos < size board -- if a valid input
                                then do pieces board ++ (pos, colour) -- add new piece to board
                                        board -- return the board
                                else Nothing -- indicating an error

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Colour
{- for every piece, try going all directions
    if any have matching colour, keep going in them until target in a row are found -> return that colour or they're empty / diff colour -> next piece
-}
checkWon board = [if checkPiece (pos,clr) pieces board target board size board /= Nothing then checkPiece (pos,clr) pieces board target board size board | (pos,clr) <- pieces board]
                    where checkPiece (pos,clr) pieces target size = do let incs = [-1...1]
                                                                        then [if checkDir x y (pos,clr) pieces target size 0 \= Nothing then checkDir x y (pos,clr) pieces target size 0 | x <- incs, y <- incs]
                                                                        else then Nothing

checkDir :: Int -> Int -> (Position, Colour) -> [(Position, Colour)] -> Int -> Int -> Maybe Colour
-- check if there are the no of same coloured pieces in a row in 1 direction from a given start point to win
-- each direction is rerpresented by a dx and dy - showing how much to add/subtract at each jump
checkDir dx dy (pos,clr) pieces target size incsDone = if not checkCoordsMatching dx dy (pos,clr) pieces (incsDone + 1) then Nothing
                                                            else if incsDone == target then Just clr
                                                            else dx dy (pos,clr) pieces target size (incsDone + 1)

checkCoordsMatching :: Int -> Int -> (Position, Colour) -> [(Position, Colour)] -> Int -> Int
-- check the pieces at 2 coords, if present, have the same colour
-- return false if there aren't pieces at both points or if the pieces at both aren't the same colour
checkCoordsMatching dx dy (pos,clr) pieces jumps = if getColourAtPos pieces (fst pos + jumps * dx, snd pos + jumps * dy) == clr

getColourAtPos :: [(Position, Colour)] -> Int -> Int -> Maybe Colour
-- get the colour of the piece at a given position; if not one there return Nothing
getColourAtPos pieces x y = [if fst pos == x && snd pos == y then clr | (pos, clr) <- pieces]
                              Nothing

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

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Colour -> Int
evaluate board clr = evaluateBoard board clr
-- just passes to method in MinimaxAI.hs, where I think function is more logical to be defined
