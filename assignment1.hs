{-
Written by: Catalin Mares
Started: 12/10/2017
Last mod: 22/10/2017
-}

-- Below are all the data types created to be used in the functions
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