-- module for the minimax AI adapted from intial code

module MinimaxAI where

import Board

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Colour,
                           next_moves :: [(Position, GameTree)] }


                           
-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (represented in Colour) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Colour -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Colour -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player

-- generate a list of possible moves to make, in the format of a list of Positions (to move to)
-- given the current Board and the Colour representing the player who's turn it is
gen :: Board -> Colour -> [Position]
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
gen board turnColour = undefined -- TODO: make this method

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depthLimit tree currentDepth = if currentDepth >= depthLimit then return Position of move with best evaulateBoard score --TODO: convert this from pseudocode
                                              else then return max of getBestMove of each move in next_moves



-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w = addMoveToWorld w movePos turn, movePos = getBestMove (buildTree gen board world turn world)
                    where addMoveToWorld :: World -> Position -> Colour -> World
                          addMoveToWorld world movePos colourTurn = pieces board world ++ (movePos, colourTurn)
                                                                    then turn world = other turn world
                                                                    then world

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



-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluateBoard :: Board -> Colour -> Int
evaluateBoard board colour = no of combos one away from target * 5 + 2 away * 2 + 3 away * 1 --TODO: convert this from pseudocode
