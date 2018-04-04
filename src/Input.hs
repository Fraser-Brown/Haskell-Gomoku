-- module to take input events, acting as an API to then call other functions in other modules to process them

module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import MinimaxAI
import Draw

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) b  = trace ("Mouse moved to: " ++ show (x,y)) b

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b = newWorld
                                                                where pos = getPos x y
                                                                      ans = makeMove (board b) (turn b) pos 
                                                                      newWorld = if nothingChecker(ans) == False then  b
                                                                                                                 else World (maybeToBoard(ans)) (other(turn b))
--handleInput (EventKey (Char k) Down _ _) b = trace ("Key " ++ show k ++ " down") b

--handleInput (EventKey (Char k) Up _ _) b = trace ("Key " ++ show k ++ " up") b

handleInput e b = b

nothingChecker:: Maybe Board -> Bool
nothingChecker Nothing = False
nothingChecker (Just x) = True

--to deal with the maybe issues
maybeToBoard:: Maybe Board -> Board
maybeToBoard (Just x) = x

getPos:: Float -> Float -> Position
getPos x y = (p, q)
             where p = round(((x + 42)/85) + 2) --will need to change 3 if board gets bigger
                   q = round(((y + 42)/85) + 2)
--TODO: Implement appropriate event handlers for input events such as clicking on the board (above); these will need to identify which board position a mouse location maps to, in particular

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}
