module Bangalore where

import Control.Monad.State.Strict

type Placement = (Int,Int)
data BState = BState
    { counter   :: Int
    , placement :: Placement
    } deriving (Show, Eq)

type Bangalore = State BState

changeCounter :: Int -> BState -> BState
changeCounter n s = s { counter = counter s + n }

initialState :: BState
initialState = BState 0 (0,0)

placeHands :: (Int,Int) -> Bangalore ()
placeHands firstTwoDigits = modify' $ \s -> s { placement = firstTwoDigits }

pressButton :: Bangalore ()
pressButton = modify' $ changeCounter 1

moveHand :: Int -> Bangalore ()
moveHand n = modify' $ changeCounter n

distance :: Int -> Int -> Int
distance 0   n    = 10 - n
distance n   0    = 10 - n
distance src dest = abs $ src - dest



-- moveAndPress :: Placement -> Int -> Int
-- find which hand is closer to next aim
-- move hand
-- press button
-- update counter
-- update Placement
-- next
