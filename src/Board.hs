-- module to model the board of the game and other elements of the game state

module Board where

data Colour = Black | White -- Colours of the pieces
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
makeMove = undefined

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Colour
checkWon = undefined

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
evaluate = undefined
