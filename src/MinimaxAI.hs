-- module for the minimax AI adapted from intial code
-- TODO: refine, test and debug the methods below

module MinimaxAI where

import Board

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }


  {-                         
-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (represented in Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
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
-- given the current Board and the Col representing the player who's turn it is
gen :: Board -> Col -> [Position]
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
gen board turnCol = do let genMoveStrategyMethods = [blockEnemyCombosMoves, addToFriendlyCombosMoves, blockPotentialEnemyCombosMoves, formnewFriendlyCombosMoves, setupPotentialFriendlyCombosMoves, allMoves]
                          for (method in genMoveStrategyMethods):
                              do let poses = method board turnCol
                              if poses /= null then poses
                          error "Couldn't generate a list of Positions as potential moves in gen() in MinimaxAI.hs, even with fallback to generate all possible moves."

genBlockEnemyCombosMoves :: Board -> Col -> [Position]
genAddToFriendlyCombosMoves :: Board -> Col -> [Position]
genBlockPotentialEnemyCombosMoves :: Board -> Col -> [Position]
genFormNewFriendlyCombosMoves :: Board -> Col -> [Position]
genSetupPotentialFriendlyCombosMoves :: Board -> Col -> [Position]
genAllMoves :: Board -> Col -> [Position]

{- STRATEGY/PRIORITY ORDER:
1. genBlockEnemyCombosMoves             ⨯  :   places to block any piece combos (of length >= 2) for opposition player
2. genAddToFriendlyCombosMoves          ⨯  :   places to add to any existing friendly combos in dir where there is space to reach target length
3. genBlockPotentialEnemyCombosMoves    ⨯  :   places to put piece in place next to individual enemy piece in a direction which could otherwise have enemy combo to reach target length
4. genFormNewFriendlyCombosMoves        ⨯  :   places to add to be 2nd item in new combo w/ another friendly piece, w/ space to reach target in at least 1 of the 2 (opposite linear) directions (from new piece to one already there and vice versa)
5. genSetupPotentialFriendlyCombosMoves ⨯  :   places to put piece in empty space where combo w/ target length can be reached in as many diff directions (of NW, N, NE...) as possible. Piece to be 1st in combo - none already there.
6. genAllMoves                          ⨯  :   get all possible moves

make list of Positions representing moves to assess for evaluation scores, constructed according to the above order
-}



-- iterating each pos fom a given piece, find first pos not in pieces - not been taken by a piece
getFirstEmptyInDirFromPos pieces pos dx dy jumpLimit = [if pieces `piecesDoesntContainPos` (x,y) then (x,y) | x = fst pos + dx * jumps, y = snd pos + dy * jumps, jumps <- [1..jumpLimit]]
                                                           Nothing -- no empty piece in dir (reaches edge of board)

-- return list of [Position] of pieces in pieces list where Col of that piece == piece arg
getListOfPosesOfColOnPieces pieces Col = do let poses = []
                                                   [if snd piece == Col then poses ++ fst piece | piece <- pieces]
                                                       poses

-- return list of poses not in pieces list by iterating over all possible poses
getListOfEmptyPosesOnPieces size pieces = do let poses = [], coordRange = [0..size - 1]
                                              [if pieces `piecesDoesntContainPos` (x,y) then poses ++ (x,y) | x <- coordRange, y <- coordRange]
                                                  poses

applyFuncInEachDirFromPos board pos func resultPoses = [if (not (dx == 0 && dy == 0)) then func board pos dx dy resultPoses | dx <- [-1..1], dy <- [-1..1]]

checkComboPossible pieces Col pos dx dy jumpLimit = [if pieces `getColAtPos` x y == other Col then False | x = fst pos + dx * jumps, y = snd pos + dy * jumps, jumps <- [1..jumpLimit]]
                                                          True





-- get list of enemy pieces on the board
--      for each enemy piece, in each dir from it:
--            check if the first piece from it is also enemy
--                  if so then return position of next empty position in that direction
genBlockEnemyCombosMoves board Col = do let enemyPieces = getListOfPosesOfColOnPieces pieces board other Col, poses = []
                                            [applyFuncInEachDirFromPos board enemyPos checkEnemyComboMove poses | enemyPos <- enemyPieces]
                                            poses
                                            where checkEnemyComboMove boardIn pos dx dy resultPoses = if not checkComboPossible pieces boardIn (pieces `getColAtPos` fst pos snd pos) pos dx dy (target boardIn - 1) then Nothing
                                                                                                      else if getColAtPos pieces boardIn fst pos + dx snd pos + dy \= other Col then [] -- if first piece in dir is friendly or empty then fail
                                                                                                           else resultPoses ++ getFirstEmptyInDirFromPos pieces boardIn pos dx dy (target boardIn - 1)

-- get list of all positions of friendly pieces on the board
--      for each piece, in each direction from it:
--            check there is space to reach combo of target length
--                  if so then return position of next empty position in that direction
genAddToFriendlyCombosMoves board Col = do let friendlyPieces = getListOfPosesOfColOnPieces pieces board Col, poses = []
                                              [applyFuncInEachDirFromPos board friendlyPos checkFriendlyComboMove poses | friendlyPos <- friendlyPieces]
                                              poses
                                              where checkFriendlyComboMove boardIn pos dx dy resultPoses = if not checkComboPossible pieces boardIn (pieces `getColAtPos` fst pos snd pos) pos dx dy (target boardIn - 1) then Nothing
                                                                                                           else resultPoses ++ getFirstEmptyInDirFromPos pieces boardIn pos dx dy (target boardIn - 1)

-- get list of all enemy pieces on the board
--      for each enemy piece, in each direction from it:
--              check there's space to reach combo of target length
--                    if so return first empty position in that dir (will be 1st from enemy piece)
genBlockPotentialEnemyCombosMoves board Col = do let enemyPieces = getListOfPosesOfColOnPieces pieces board other Col, poses = []
                                                    [applyFuncInEachDirFromPos board enemyPos checkEnemyComboMove poses | enemyPos <- enemyPieces]
                                                    poses
                                                    where checkEnemyComboMove boardIn pos dx dy resultPoses = if not checkComboPossible pieces boardIn (pieces `getColAtPos` fst pos snd pos) pos dx dy (target boardIn - 1) then Nothing
                                                                                                              else resultPoses ++ getFirstEmptyInDirFromPos pieces boardIn pos dx dy (target boardIn - 1)

-- get list of all friendly pieces on the board
--      for each friendly piece, in each dir from it:
--          check pos at first step from it is empty
--          check there's space in dir or reverse to form combo of target length
--                if both pass return pos of first step from start
genFormNewFriendlyCombosMoves board Col = do let friendlyPieces = getListOfPosesOfColOnPieces pieces board Col, poses = []
                                                [applyFuncInEachDirFromPos board friendlyPos checkFormNewFriendlyCombosMove poses | friendlyPos <- friendlyPieces]
                                                poses
                                                where checkFormNewFriendlyCombosMove boardIn pos dx dy resultPoses = if pieces `piecesContainsPos` fst pos + dx snd pos + dy then Nothing
                                                                                                                     else if not checkComboPossible pieces boardIn (pieces `getColAtPos` fst pos snd pos) pos dx dy (target boardIn - 1) then Nothing
                                                                                                                               else resultPoses ++ (fst pos + dx, snd pos + dy)

-- find list of empty pieces
--      for each empty piece, in each dir from it:
--            check how many empty/friendly pieces there are for steps up to target no away
--                  find how many dirs could become a friendly combo
--                  if none, find how many opposite dirs could combine to combo of target length (target / 2 each)
--                  if either above pass and pos being checked not already in list of moves, add to it
genSetupPotentialFriendlyCombosMoves board Col = do let emptyPieces = getListOfEmptyPosesOnPieces size board pieces board, poses = []
                                                    [applyFuncInEachDirFromPos board emptyPos checkSetupPotentialFriendlyCombo poses | emptyPos <- emptyPieces]
                                                    poses
                                      
checkSetupPotentialFriendlyCombo :: Board -> Position -> Int -> Int -> [Position] -> Void
--check how many empty/friendly pieces there are for steps up to target no away
--      find how many dirs could become a friendly combo
--      if none, find how many opposite dirs could combine to combo of target length (target / 2 each)
--      if either above pass and pos being checked not already in list of moves, add to it
checkSetupPotentialFriendlyCombo board pos dx dy resultPoses = do let pieces = pieces board, Col = (pieces `getColAtPos` fst pos snd pos), target = target board
                                                                  if checkComboPossible pieces board Col pos dx dy (target boardIn - 1) && not (elem pos resultPoses) then resultPoses ++ pos
                                                                  else -- not possible to form combo in dir given, so try to find if possible in conjunction w/ opposite direction
                                                                      do let limitForward = 0, limitBack = 0
                                                                      do [if checkComboPossible pieces Col pos dx dy jumps then limitForward = jumps | jumps <- [1..target - 1]]
                                                                      do [if checkComboPossible pieces Col pos negate dx negate dy jumps then limitBack = jumps | jumps <- [1..target - 1]]
                                                                      if limitForward + limitBack + 1 >= target then resultPoses ++ pos


genAllMoves board Col = do let pieces = pieces board, poses = [], coordRange = [0 .. size board - 1]
                               [poses ++ pos if pieces `doesntContainPos` pos | pos = (x,y), x <- coordRange, y <- coordRange]
                               poses

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depthLimit tree = do let poses = [fst move | move <- next_moves tree], scores = [getMaxEvalScore depthLimit (snd move) 1 game_turn tree | move <- next_moves tree]
                                  poses !! elemIndex (max scores) scores
  
  
getMaxEvalScore :: Int -> GameTree -> Position -> Col -> Int
getMaxEvalScore depthLimit tree currentDepth Col = if currentDepth >= depthLimit then return evaluate game_board tree Col
                                                         else max [getMaxEvalScore depthLimit currTree currentDepth + 1 Col | currTree = snd move, move <- next_moves tree]

-}
-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w = w            
{-updateWorld t w = addMoveToWorld w movePos turn, movePos = getBestMove (buildTree gen board world turn world)
                    where addMoveToWorld :: World -> Position -> Col -> World
                          addMoveToWorld world movePos ColTurn = pieces board world ++ (movePos, ColTurn)
                                                                    turn world = other turn world
                                                                    world
-}
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