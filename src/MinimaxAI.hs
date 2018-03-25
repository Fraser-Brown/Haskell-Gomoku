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
gen board turnColour = do let genMoveStrategyMethods = [blockEnemyCombosMoves, addToFriendlyCombosMoves, blockPotentialEnemyCombosMoves, formnewFriendlyCombosMoves, setupPotentialFriendlyCombosMoves, allMoves]
                           then [if poses /= null then poses | poses = method board turnColour, method <- genMoveStrategyMethods]
                           then error "Couldn't generate a list of Positions as potential moves in gen() in MinimaxAI.hs, even with fallback to generate all possible moves."

-- data Board = Board { size :: Int,
--                      target :: Int,
--                      pieces :: [(Position, Colour)]
--                    }

genBlockEnemyCombosMoves :: Board -> Colour -> [Position]
genAddToFriendlyCombosMoves :: Board -> Colour -> [Position]
genBlockPotentialEnemyCombosMoves :: Board -> Colour -> [Position]
genFormNewFriendlyCombosMoves :: Board -> Colour -> [Position]
genSetupPotentialFriendlyCombosMoves :: Board -> Colour -> [Position]
genAllMoves :: Board -> Colour -> [Position]
--TODO: implement the above methods.

{- STRATEGY/PRIORITY ORDER:
1. genBlockEnemyCombosMoves             ~  :   places to block any piece combos (of length >= 2) for opposition player
2. genAddToFriendlyCombosMoves          ⨯  :   places to add to any existing friendly combos in dir where there is space to reach target length
3. genBlockPotentialEnemyCombosMoves    ⨯  :   places to put piece in place next to individual enemy piece in a direction which could otherwise have enemy combo to reach target length
4. genFormNewFriendlyCombosMoves        ⨯  :   places to add to be 2nd item in new combo w/ another friendly piece, w/ space to reach target in at least 1 of the 2 (opposite linear) directions (from new piece to one already there and vice versa)
5. genSetupPotentialFriendlyCombosMoves ⨯  :   places to put piece in empty space where combo w/ target length can be reached in as many diff directions (of NW, N, NE...) as possible. Piece to be 1st in combo - none already there.
6. genAllMoves                          ✓  :   get all possible moves

make list of Positions representing moves to assess for evaluation scores, constructed according to the above order
-}
genBlockEnemyCombosMoves board colour = do let pieces = pieces board, target = target board, coordRange = [0 .. size board - 1], moves = []
-- check all enemy pieces; look in all directions. if there is a piece/pieces in that direction of enemy colour, and remaining space to reach target, add move to put piece in next space
                                            [if snd piece \= colour moves ++ getBlockEnemyCombosMovesFromEnemyPiece piece pieces target | piece <- pieces]
                                                then moves
                                                where getBlockEnemyCombosMovesFromEnemyPiece piece pieces = do let deltas = [-1..1], results = []
                                                                                                                --then [if pieces contains piece in direction && its colour \= colour find next in that dir that's empty while colour on way \= colour and in range then return first empty]
                                                                                                                then [if pieces `piecesContainsPos` (x, y) && getColourAtPos pieces x y \= colour then results ++ getNextEmptyIfCouldFormCombo x y pieces other colour coordRange target dx dy | x <- coordRange, y <- coordRange, dx <- deltas, dy <- deltas]

                                                                                                                -- find next in that dir that's empty while colour on way \= colour and in range then return first empty
                                                                                                                    where getNextEmptyIfCouldFormCombo x y pieces enemyColour coordRange target dx dy = if not pieces `piecesContainsPos` (x + dx * (target - 1), y + dy * (target - 1)) then [] -- if couldn't reach target in dir then return empty list
                                                                                                                                                                                                            else -- if could form combo of target in dir if all are empty/of enemy colour - i.e. assuming no friendly pieces in that dir
                                                                                                                                                                                                                --then if any pieces in dir within target range are of friendly colour then []
                                                                                                                                                                                                                then [if not (dx == 0 && dy == 0) && pieces `piecesContainsPos` (currX, currY) && getColourAtPos pieces currX currY \= enemyColour then [] | currX = x + jumps * dx, currY = y + jumps * dx, jumps = [1..(target - 1)]]
                                                                                                                                                                                                                    -- then we know the enemy could get a combo of target length in this dir, so get first empty in dir given
                                                                                                                                                                                                                    then [if not (dx == 0 && dy == 0) && pieces `piecesDoesntContainPos` (currX, currY) then (currX, currY) | currX = x + jumps * dx, currY = y + jumps * dx, jumps = [1..(target - 1)]]

genAllMoves board colour = do let pieces = pieces board, poses = [], coordRange = [0 .. size board - 1]
                               then [poses ++ pos if pieces `doesntContainPos` pos | pos = (x,y), x <- coordRange, y <- coordRange]
                               then poses

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depthLimit tree = do let poses = [fst move | move <- next_moves tree], scores = [getMaxEvalScore depthLimit (snd move) 1 game_turn tree | move <- next_moves tree]
                                  then poses !! elemIndex (max scores) scores
  
  
getMaxEvalScore :: Int -> GameTree -> Position -> Colour -> Int
getMaxEvalScore depthLimit tree currentDepth colour = if currentDepth >= depthLimit then return evaluate game_board tree colour
                                                         else then max [getMaxEvalScore depthLimit currTree currentDepth + 1 colour | currTree = snd move, move <- next_moves tree]


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