{-
Written by: Catalin Mares
Started: 12/11/2017
Last mod: 19/11/2017
-}

import Data.List
import System.Random 
import Debug.Trace

--Declaring data types and variables that are used to initialise the dominoes round later
type DomsPlayer = Board -> Hand -> [(Domino,End)]

data Turn = SIMPLE | SMART
  deriving(Eq,Show)
initTurn :: Turn 
initTurn = SIMPLE

type Score = (Int,Int)
initScore :: Score
initScore = (0,0)

emptyBoard :: Board
emptyBoard = []

--Over here I am generating 28 random numbers, combining each domino with a number, sorting the list according to the random number
--and then extracting from each tuple created just the domino itself. This will basically shuffle the 28 dominoes 
shuffleDoms :: Int -> Hand
shuffleDoms x = map snd (sort(zip ranList allDom))
   where ranList = take 28 (randoms (mkStdGen x):: [Int])

--Simple Player will look at each domino in the hand starting from the first element and play the first element encountered
--which can be played on the given board 
simplePlayer :: DomsPlayer
simplePlayer [] (h:t) = [(h,LEFT)]
simplePlayer b (h:t)
  | (goesP b h LEFT) = [(h,LEFT)]
  | (goesP b h RIGHT) = [(h,RIGHT)] 
  | otherwise = simplePlayer b t  
 
--This uses a few functions that just return the highest scoring domino and use it to see if the domino goes on the left or on the right
hsdPlayer :: DomsPlayer
hsdPlayer b h 
  | (goesP b (extractDom b h) LEFT) && (goesP b (extractDom b h) RIGHT) = chooseHighest b (extractDom b h)
  | (goesP b (extractDom b h) LEFT) = [((extractDom b h),LEFT)]
  | (goesP b (extractDom b h) RIGHT) = [((extractDom b h),RIGHT)]

--If a domino can be played on both ends this will choose which end will give the highest score 
chooseHighest :: Board -> Domino -> [(Domino,End)]
chooseHighest b d
  | scoreBoard z > scoreBoard x =  [(d,LEFT)]
  | otherwise = [(d,RIGHT)]
     where Just z = playDom b d LEFT; Just x = playDom b d RIGHT

--Here I am extracting just the domino from the sorted list of [(Int,Domino)] which is the  list of the scores all the dominoes give when played
extractDom :: Board -> Hand -> Domino
extractDom b h = snd (last(sort(scores b h 0)))
 
--This computes a list [(Int,Domino)] with the scores given by the dominoes that can be played on the board 
scores :: Board -> Hand -> Int -> [(Int,Domino)]
scores b [] d = []
scores b (h:t) m  
  | (goesP b h RIGHT) && (goesP b h LEFT) && ((scoreBoard (z) >= m) || (scoreBoard (x) >= m)) = [((maximumBy compare [(scoreBoard (z)),(scoreBoard (x))]),h)]++scores b t (maximumBy compare [(scoreBoard (z)),(scoreBoard (x))])
  | (goesP b h RIGHT) && (scoreBoard (x) >= m) = [((scoreBoard (x)),h)]++(scores b t (scoreBoard (x)))
  | (goesP b h LEFT) && (scoreBoard (z) >= m) = [((scoreBoard (z)),h)]++(scores b t (scoreBoard (z)))
  | otherwise =  scores b t m
      where Just z = playDom b h LEFT; Just x = playDom b h RIGHT

--Using a different helper function to compute the final score. Giving it an initial empty board, two hands, initial turn and initial score (0,0)
playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> Score
playDomsRound a b c = startPlay emptyBoard  (simpleHand c) (smartHand c) initTurn initScore 

--Alternating the turn between the players, each time updating the hand, board, score and changing the player's turn after a player plays
startPlay :: Board -> Hand -> Hand -> Turn -> Score -> Score
startPlay b siH smH t s 
  | (knockingP b siH) && (knockingP b smH) = s
  | (t == SIMPLE) && (knockingP b siH) = startPlay b siH smH (changeTurn t) s
  | (t == SMART) && (knockingP b smH) = startPlay b siH smH (changeTurn t) s
  | (t == SIMPLE) = startPlay (updateBoard b (simplePlayer b siH)) (updateHand siH (simplePlayer b siH)) smH (changeTurn t) (updateScore b t siH s)
  | (t == SMART)  = startPlay (updateBoard b (hsdPlayer b smH)) siH (updateHand smH (hsdPlayer b smH)) (changeTurn t) (updateScore b t smH s)  
  | otherwise = s

--Updating the board with the move made 
updateBoard :: Board -> [(Domino,End)] -> Board
updateBoard b [] =  b
updateBoard b d =  n
  where [(x,e)] = d; Just n = playDom b x e

--Updating the score after a move has been made 
updateScore :: Board -> Turn -> Hand -> (Int,Int) -> (Int,Int)
updateScore b t h (x,y)
  | (t == SIMPLE) = (x+(scoreBoard (updateBoard b (simplePlayer b h))),y)
  | (t == SMART) = (x,y+(scoreBoard (updateBoard b (hsdPlayer b h)))) 

--Generating a hand for the simple player by taking the first 9 dominoes from the shuffled list of dominoes 
simpleHand :: Int -> Hand
simpleHand x = take 9(shuffleDoms x)

--Generating a hand for the smart player by taking the second set of 9 dominoes from the suffled list of dominoes 
smartHand :: Int -> Hand
smartHand x = take 9(drop 9(shuffleDoms x))

--Alternating the turn between the players 
changeTurn :: Turn -> Turn
changeTurn x
  | x == SIMPLE = SMART
  | x == SMART = SIMPLE

--Updating the hand of the player which has just played by taking out the domino he has played form his hand
updateHand :: Hand -> [(Domino,End)] ->  Hand
updateHand h [] = h
updateHand [] _ = []
updateHand h [(x,_)] = filter ( \n -> n /=x  ) h


--Code from Assignment1 is below

type Domino = (Int,Int)

type Hand = [Domino]
allDom :: Hand
allDom = [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4),(2,5),(2,6),(3,3),(3,4),(3,5),(3,6),(4,4),(4,5),(4,6),(5,5),(5,6),(6,6)]

type Board = [Domino]
 
data End = LEFT | RIGHT
  deriving(Eq,Show)

-- If the end is left check if domino given matches the head of the board
-- If the end is right check if domino given matches the tail of the board
goesP :: Board->Domino->End->Bool
goesP [] _ _ = True
goesP (h:t) (a,b) e
  | ((e == RIGHT) && (length(h:t)/=1)) = goesP [last(h:t)] (a,b) RIGHT
  | (e == RIGHT) && (a==y || b==y) = True
  | (e == LEFT) && (a==x || b==x) = True
  | otherwise = False
      where (x,y) = h

-- Check if each domino in the hand can either be played on the left or right of the board
knockingP :: Board->Hand->Bool
knockingP _ [] = True
knockingP [] _ = False
knockingP b (h:t)
  | goesP b h LEFT = False
  | goesP b h RIGHT = False
  | otherwise = knockingP b t

-- Check if the domino given already exists on the board
playedP :: Board->Domino->Bool
playedP [] _ = False
playedP (h:t) (a,b)
  | ((x==a) && (y==b)) || ((y==a) && (x==b)) = True
  | otherwise = playedP t (a,b) 
      where(x,y) = h

-- Check if each domino in the hand can go left or right of the board
-- Check each domino against the head and tail of the board  
possPlays :: Board->Hand->[(Domino,End)]
possPlays b [] = []
possPlays b (h:t)
  | (goesP b h LEFT) && (goesP b h RIGHT) = [(h,RIGHT),(h,LEFT)]++(possPlays b t)
  | (goesP b h RIGHT) = (h,RIGHT):(possPlays b t)
  | (goesP b h LEFT) = (h,LEFT):(possPlays b t)
  | otherwise = possPlays b t

-- Take in the domino and play it at the head if the end is left
-- Take in the domino and play it at the tail if the end is right
-- Play the domino (x,y) or (y,x) depending on what the head or tail looks like  
playDom :: Board->Domino->End-> Maybe Board 
playDom [] d _ = Just [d]
playDom b (x,y) e 
  | (goesP b (x,y) e) && (p==y) && (e==LEFT) = Just ((x,y):b)
  | (goesP b (x,y) e) && (p==x) && (e==LEFT)= Just ((y,x):b)
  | (goesP ([last(b)]) (x,y) e) && (l==y) = Just ((b)++[(y,x)])
  | (goesP ([last(b)]) (x,y) e) && (l==x) = Just ((b)++[(x,y)])
  | otherwise = Nothing
      where (p,o) = head(b); (k,l) = last(b)

-- If only one element exists do mod 5 and mod 3 of the sum of the two numbers of the domino (x,y)
-- If the mod 5 or 3 comes to 0 then divide the sum by the appropriate mod int which gave the result 0 
-- If either the domino at the head or tail is symmetrical (x,x) then it will count as a whole 2*x rather than just x
scoreBoard :: Board->Int
scoreBoard [] = 0
scoreBoard (h:t)
  |  length(h:t) == 1 &&  ((mod (x+y) 3) == 0) = div (x+y) 3
  |  length(h:t) == 1 &&  ((mod (x+y) 5) == 0) = div (x+y) 5
  |  length(h:t) > 1 && (x==y) && ((mod (x+y+l) 3) == 0) && ((mod (x+y+l) 5) == 0) = (div (x+y+l) 3) + (div (x+y+l) 5)
  |  length(h:t) > 1 && (k==l) && ((mod (x+k+l) 3) == 0) && ((mod (x+k+l) 5) == 0) = (div (x+k+l) 3) + (div (x+k+l) 5)
  |  length(h:t) > 1 && (x==y) && (k==l) && ((mod (x+y+k+l) 3) == 0) = (div (x+y+k+l) 3)
  |  length(h:t) > 1 && (x==y) && (k==l) && ((mod (x+y+k+l) 5) == 0) = (div (x+y+k+l) 5)
  |  length(h:t) > 1 && (x==y) && ((mod (x+y+l) 3) == 0) = (div (x+y+l) 3)
  |  length(h:t) > 1 && (x==y) && ((mod (x+y+l) 5) == 0) = (div (x+y+l) 5)
  |  length(h:t) > 1 && (k==l) && ((mod (k+l+x) 3) == 0) = (div (k+l+x) 3)
  |  length(h:t) > 1 && (k==l) && ((mod (k+l+x) 5) == 0) = (div (k+l+x) 5)      
  |  length(h:t) > 1 && (k==l) && (x/=y) && ((mod (x+k+l) 3) == 0) = (div (x+k+l) 3)
  |  length(h:t) > 1 && (k==l) && (x/=y) && ((mod (x+k+l) 5) == 0) = (div (x+k+l) 5)  
  |  (length(h:t) > 1) && (x/=y) && (k/=l) && (mod (x+l) 3 == 0) = div (x+l) 3
  |  (length(h:t) > 1) && (x/=y) && (k/=l) && (mod (x+l) 5 == 0) = div (x+l) 5
  |  otherwise = 0
       where (x,y) = h; (k,l) = last((h:t))

-- Compute the output using another function which takes in more parameters
-- allDom represents a variable that contains all the dominoes possible
scoreN :: Board->Int->[(Domino,End)]
scoreN [] 2 = [((3,3),LEFT),((3,3),RIGHT),((5,5),LEFT),((5,5),RIGHT),((1,5),LEFT),((1,5),LEFT)]
scoreN [] 1 = [((0,3),LEFT),((0,3),RIGHT),((0,5),LEFT),((0,5),RIGHT)]
scoreN [] _ = []
scoreN b x = makeHand allDom b x

-- The hand becomes all the dominoes possible to be played on the board  
-- Iterate through each domino and see if it can be played on the left or on the right 
-- Only play a domino if it is not already on the board, its score matches and it can be played at an end  
makeHand :: Hand -> Board -> Int ->[(Domino,End)]
makeHand [] b c = []
makeHand (h:t) b c
  |  (goesP b h LEFT) && (goesP b h RIGHT) && ((playedP b (h))==False) && ((scoreBoard (z)) == c) && ((scoreBoard x) == c) = [(h,LEFT),(h,RIGHT)]++(makeHand t b c) 
  |  (goesP b h LEFT) && ((playedP b (h))==False) && ((scoreBoard (z)) == c) = [(h,LEFT)]++(makeHand t b c) 
  |  (goesP b h RIGHT) && ((playedP b (h))==False) &&  ((scoreBoard (x)) == c) = [(h,RIGHT)]++(makeHand t b c) 
  |  otherwise = makeHand t b c
       where Just z = playDom b h LEFT; Just x = playDom b h RIGHT