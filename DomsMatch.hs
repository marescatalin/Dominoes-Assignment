module DomsMatch where

 {- play a 5's & 3's singles match between 2 given players
    play n games, each game up to 61
 -}
 
 --import Doms
 import System.Random
 import Data.List
 import Debug.Trace
 
 type Dom = (Int,Int)
 -- with highest pip first i.e. (6,1) not (1,6)

 data DomBoard = InitBoard|Board Dom Dom History
                    deriving (Eq,Show)
 
 type History = [(Dom,Player,MoveNum)]
 -- History allows course of game to be reconstructed                                            
                                               
 data Player = P1|P2 -- player 1 or player 2
                  deriving (Eq,Show)
 
 data End = L|R -- left end or right end
                  deriving (Eq,Show)
 
 type MoveNum = Int

 type Hand = [Dom]
  
 -- the full set of Doms
 domSet :: [Dom]
 
 domSet = [(6,6),(6,5),(6,4),(6,3),(6,2),(6,1),(6,0),
                 (5,5),(5,4),(5,3),(5,2),(5,1),(5,0),
                       (4,4),(4,3),(4,2),(4,1),(4,0),
                             (3,3),(3,2),(3,1),(3,0),
                                   (2,2),(2,1),(2,0),
                                         (1,1),(1,0),
                                               (0,0)]  
                                                                                         
 
 type Move = (Dom,End)
 type Scores = (Int,Int)
                                                                                              
 -- state in a game - p1's hand, p2's hand, player to drop, current board, scores 
 type GameState =(Hand,Hand,Player, DomBoard, Scores)
 
 
 ------------------------------------------------------
 {- DomsPlayer
    given a Hand, the Board, which Player this is and the current Scores
    returns a Dom and an End
    only called when player is not knocking
    made this a type, so different players can be created
 -}
 
 type DomsPlayer = Hand->DomBoard->Player->Scores->(Dom,End)
 
 {- variables
     hand h
     board b
     player p
     scores s
 -}

 -- example players
 -- randomPlayer plays the first legal dom it can, even if it goes bust
 randomPlayer :: DomsPlayer
 
 randomPlayer h b p s 
   |not(null ldrops) = ((head ldrops),L)
   |otherwise = ((head rdrops),R)
  where
   ldrops = leftdrops h b
   rdrops = rightdrops h b
  
-------------------------------------------------------------------------
------------------------------------------------------------------------
--Smartest player
 smartPlayer :: DomsPlayer
 smartPlayer h b p (x,y)
   | (p==P1) && winGame h b x  = giveWinner h b x   
   | (p==P2) && winGame h b y = giveWinner h b y 
   | firstDropPossible h b = ((5,4),L) 
   | (canPreventLoss==True)  = hsdPlayer [v] b p (x,y)
   | (p==P1) && getTo59 h b x  = getDomTo59 h b x 
   | (p==P2) && getTo59 h b y = getDomTo59 h b y  
   | (q `elem` n) && not(anyPipInHand h) && not(knocking (filterHand h q) b) = hsdPlayer (filterHand h q) b p (x,y)
   | (s==d) && (endsX0 b q) && not(knocking (filterHand h q) b) && not(safe0X h b q) =  hsdPlayer (filterHand h q) b p (x,y)
   | (playMajorityPip h) && (pipMajorityLength h z) && not(knocking (findMCDom h) b) && ((checkEndsHP m b k)) = hsdPlayer (findMCDom h) b p (x,y)
   | otherwise = (q,w)
      where (q,w) = hsdPlayer h b p (x,y)
            (s,d) = q; z= findBoard b
            k = findMajorityPip h
            m= hsdPlayer (findMCDom h) b p (x,y)
            n = [(6,6),(5,5),(4,4),(3,3),(2,2),(1,1),(0,0)] 
            (canPreventLoss,v) = if (p==P1) then (checkPreventLoss h b y) else (checkPreventLoss h b x)
            
 
---------------------------------------------------------------------------------
----------------------------------------------------------------------------------
--From here onwards I have some variations of smart players, this is done by moving
--tactics around and adding or removing tactics from use
 smartPlayer1 :: DomsPlayer
 smartPlayer1 h b p (x,y)
   | firstDropPossible h b = ((5,4),L)
   | (p==P1) && winGame h b x  = giveWinner h b x   
   | (p==P2) && winGame h b y = giveWinner h b y
   | (p==P1) && getTo59 h b x  = getDomTo59 h b x 
   | (p==P2) && getTo59 h b y = getDomTo59 h b y  
   | (canPreventLoss==True)  = hsdPlayer [v] b p (x,y)  
   | (q `elem` n) && not(anyPipInHand h) && not(knocking (filterHand h q) b) = hsdPlayer (filterHand h q) b p (x,y)
   | (s==d) && (endsX0 b q) && not(knocking (filterHand h q) b) && not(safe0X h b q) =  hsdPlayer (filterHand h q) b p (x,y)
   | (playMajorityPip h) && (pipMajorityLength h z) && not(knocking (findMCDom h) b) && ((checkEndsHP m b k)) = hsdPlayer (findMCDom h) b p (x,y)
   | otherwise = (q,w)
      where (q,w) = hsdPlayer h b p (x,y)
            (s,d) = q; z= findBoard b
            k = findMajorityPip h
            m= hsdPlayer (findMCDom h) b p (x,y)
            n = [(6,6),(5,5),(4,4),(3,3),(2,2),(1,1),(0,0)] 
            (canPreventLoss,v) = if (p==P1) then (checkPreventLoss h b y) else (checkPreventLoss h b x) 
           
      
 

 smartPlayer2 :: DomsPlayer
 smartPlayer2 h b p (x,y)
   | firstDropPossible h b = ((5,4),L) 
   | otherwise = (q,w)
      where (q,w) = hsdPlayer h b p (x,y)
            (s,d) = q; z= findBoard b
            k = findMajorityPip h
            m= hsdPlayer (findMCDom h) b p (x,y)
            n = [(6,6),(5,5),(4,4),(3,3),(2,2),(1,1),(0,0)] 
            (canPreventLoss,v) = if (p==P1) then (checkPreventLoss h b y) else (checkPreventLoss h b x)

 smartPlayer3 :: DomsPlayer
 smartPlayer3 h b p (x,y)
   | firstDropPossible h b = ((5,4),L)
   | (p==P1) && winGame h b x  = giveWinner h b x   
   | (p==P2) && winGame h b y = giveWinner h b y
   | (p==P1) && getTo59 h b x  = getDomTo59 h b x 
   | (p==P2) && getTo59 h b y = getDomTo59 h b y  
   | otherwise = (q,w)
      where (q,w) = hsdPlayer h b p (x,y)
            (s,d) = q; z= findBoard b
            k = findMajorityPip h
            m= hsdPlayer (findMCDom h) b p (x,y)
            n = [(6,6),(5,5),(4,4),(3,3),(2,2),(1,1),(0,0)] 
            (canPreventLoss,v) = if (p==P1) then (checkPreventLoss h b y) else (checkPreventLoss h b x)

 smartPlayer4 :: DomsPlayer
 smartPlayer4 h b p (x,y)
   | firstDropPossible h b = ((5,4),L)
   | (p==P1) && winGame h b x  = giveWinner h b x   
   | (p==P2) && winGame h b y = giveWinner h b y
   | (p==P1) && getTo59 h b x  = getDomTo59 h b x 
   | (p==P2) && getTo59 h b y = getDomTo59 h b y  
   | (canPreventLoss==True)  = hsdPlayer [v] b p (x,y)  
   | otherwise = (q,w)
      where (q,w) = hsdPlayer h b p (x,y)
            (s,d) = q; z= findBoard b
            k = findMajorityPip h
            m= hsdPlayer (findMCDom h) b p (x,y)
            n = [(6,6),(5,5),(4,4),(3,3),(2,2),(1,1),(0,0)] 
            (canPreventLoss,v) = if (p==P1) then (checkPreventLoss h b y) else (checkPreventLoss h b x)

 smartPlayer5 :: DomsPlayer
 smartPlayer5 h b p (x,y)
   | firstDropPossible h b = ((5,4),L)
   | (p==P1) && winGame h b x  = giveWinner h b x   
   | (p==P2) && winGame h b y = giveWinner h b y
   | (p==P1) && getTo59 h b x  = getDomTo59 h b x 
   | (p==P2) && getTo59 h b y = getDomTo59 h b y  
   | (canPreventLoss==True)  = hsdPlayer [v] b p (x,y)  
   | (q `elem` n) && not(anyPipInHand h) && not(knocking (filterHand h q) b) = hsdPlayer (filterHand h q) b p (x,y)
   | otherwise = (q,w)
      where (q,w) = hsdPlayer h b p (x,y)
            (s,d) = q; z= findBoard b
            k = findMajorityPip h
            m= hsdPlayer (findMCDom h) b p (x,y)
            n = [(6,6),(5,5),(4,4),(3,3),(2,2),(1,1),(0,0)] 
            (canPreventLoss,v) = if (p==P1) then (checkPreventLoss h b y) else (checkPreventLoss h b x)

 smartPlayer6 :: DomsPlayer
 smartPlayer6 h b p (x,y)
   | firstDropPossible h b = ((5,4),L)
   | (p==P1) && winGame h b x  = giveWinner h b x   
   | (p==P2) && winGame h b y = giveWinner h b y
   | (p==P1) && getTo59 h b x  = getDomTo59 h b x 
   | (p==P2) && getTo59 h b y = getDomTo59 h b y  
   | (canPreventLoss==True)  = hsdPlayer [v] b p (x,y)  
   | (q `elem` n) && not(anyPipInHand h) && not(knocking (filterHand h q) b) = hsdPlayer (filterHand h q) b p (x,y)
   | (s==d) && (endsX0 b q) && not(knocking (filterHand h q) b) && not(safe0X h b q) =  hsdPlayer (filterHand h q) b p (x,y)
   | otherwise = (q,w)
      where (q,w) = hsdPlayer h b p (x,y)
            (s,d) = q; z= findBoard b
            k = findMajorityPip h
            m= hsdPlayer (findMCDom h) b p (x,y)
            n = [(6,6),(5,5),(4,4),(3,3),(2,2),(1,1),(0,0)] 
            (canPreventLoss,v) = if (p==P1) then (checkPreventLoss h b y) else (checkPreventLoss h b x)

 
            
 
------------------------------------------------------------------------------
-----------------------------------------------------------------------------
{- Get Domino to 59 and Win game helper function tactics are shown below
    given a hand, a board and the current score of the player who is currently
    playing, these functions will work out if there exists a domino in the hand
    that can be played on the current board in order to win the game or if that 
    is not possible, it will look for a domino in the hand that can be played in 
    order to make the score 59 as there are more ways to score 2 
 -}

 --This function gives the domino from the hand that can be played to make the score 59
 getDomTo59 :: Hand->DomBoard->Int->(Dom,End)
 getDomTo59 h b x = head(makeHand h b (59-x))

--This function checks if there exist a domino in hand that can be played to make the score 59
 getTo59 :: Hand->DomBoard->Int->Bool
 getTo59 h b x 
   | (x>50) && ((makeHand h b (59-x))/=[]) = True
   | otherwise = False

--This function gives the domino from the hand that can be played to win the game
 giveWinner :: Hand->DomBoard->Int->(Dom,End)
 giveWinner h b x = head(makeHand h b (61-x))

--This function checks if there exists a domino in the hand that can be played to win the game
 winGame :: Hand->DomBoard->Int->Bool
 winGame h b x
   | (x>52) && ((makeHand h b (61-x))/=[]) = True
   | otherwise = False   

--Function given a hand, a board and an int, gives all the dominoes in the hand that score that int
--and the ends those dominoes can be played at
 makeHand :: Hand -> DomBoard -> Int -> [(Dom,End)]
 makeHand [] b c = []
 makeHand (h:t) b c
  |  (goesLP h b) && ((scoreboard (z)) == c) = [(h,L)]
  |  (goesRP h b) && ((scoreboard (x)) == c) = [(h,R)]
  |  otherwise = makeHand t b c
       where Just z = playDom P1 h L b; Just x = playDom P1 h R b


------------------------------------------------------------------------------
-----------------------------------------------------------------------------
{- Try to prevent the opponent from winning the game
    if the opponents score is bigger than 52 then we will start checking if the next
    move that we are about to make is safe so that the opponent doesn't win the game
 -}
--This function checks if the current move is safe to make and returns True and the domino
--from the hand which is safe to play so the opponent doesn't win
 checkPreventLoss :: Hand -> DomBoard -> Int -> (Bool,Dom)
 checkPreventLoss [] _ _  = (False,(0,0))
 checkPreventLoss h z s
   | not(knocking h z) && not(preventLoss w (hsdPlayer h z P1 (0,0)) z s) = (True,fst(hsdPlayer h z P1 (0,0)))
   | knocking h z = (False,(0,0))
   | otherwise = checkPreventLoss w z s 
      where w = filterHand h (fst(hsdPlayer h z P1 (0,0)))
            
--This functions will find out if the opponent can win on their next move if we have made
--the move that we were about to make, if they can win it will return true          
 preventLoss :: Hand -> (Dom,End) -> DomBoard -> Int -> Bool
 preventLoss h (d,e) z s 
   | (winGame (findPossibleOppH h b) b s) = True
   | otherwise = False
      where b = updateBoard d e P1 z

--Function to invert dominoes so that they are in the same format as domSet
 invertDoms :: Hand -> Hand
 invertDoms h = map (\x -> if (fst(x)<snd(x)) then swap x else x) h

--Function to swap a tuple around
 swap :: (Int, Int) -> (Int, Int)
 swap (a,b) = (b,a)

--Function to extract the board from the history
 findBoard :: DomBoard -> Hand
 findBoard InitBoard = []
 findBoard  (Board y x hist) = map extractFirst hist

--Function to predict the opponents possible hand
 findPossibleOppH :: Hand -> DomBoard -> Hand
 findPossibleOppH h b = domSet \\ (h++(invertDoms((findBoard b))))

------------------------------------------------------------------------------
-----------------------------------------------------------------------------
{- If you have first drop and you have (5,4) in your hand then play it
 -}
--This function checks if the board is empty and if you have (5,4) in your hand
 firstDropPossible :: Hand -> DomBoard -> Bool
 firstDropPossible h b 
   | (b==InitBoard) = ((5,4) `elem` h)
   | otherwise = False

------------------------------------------------------------------------------
-----------------------------------------------------------------------------
{- If your highest scoring domino is any double then check that the opponent can score
more than you once you play that domino. If they can't score higher than you then play 
that domino, otherwise play your second highest scoring domino.
 -}
--This function checks that the ends are 0 something. In this case l=r so we only
--check for l to be the same as one of the ends and the other end bust be 0
 endsX0 :: DomBoard -> Dom -> Bool
 endsX0 InitBoard _  = False
 endsX0 (Board (x,_) (_,b) hist) (l,r)
   | ((x/=b) && (x==l) && (b==0)) || ((x/=b) && (b==l) && (x==0)) = True
   | otherwise = False
           
--If it is safe to play a double (opponent is unable to score more than you once you play)
--then return true, otherwise return false    
 safe0X ::  Hand  -> DomBoard -> Dom -> Bool
 safe0X h b d
   | (d==(6,6)) = safe06 h b
   | (d==(5,5)) = safe05 h b
   | (d==(4,4)) = safe04 h b
   | (d==(3,3)) = safe06 h b
   | (d==(2,2)) = safe02 h b
   | (d==(1,1)) = safe01 h b
   | otherwise = False

--Checks if it is safe to play (6,6) so that opponent is unable to score more once you play it
 safe06 ::  Hand  -> DomBoard -> Bool
 safe06 h (Board (x,_) (_,b) hist)
   | ((0,3)`elem` m || (3,0) `elem` m) && ((0,6)`elem` h || (6,0) `elem` h)  = True
   | ((0,6)`elem` m || (6,0) `elem` m) && ((0,3)`elem` h || (3,0) `elem` h)  = True
   | (((0,6) `elem` m) || ((6,0) `elem` m))  && (((0,3) `elem` m) || ((3,0) `elem` m))  = True
   | (((0,6) `elem` h) || ((6,0) `elem` h))  && (((0,3) `elem` h) || ((3,0) `elem` h))  = True
   | otherwise = False
      where m = map extractFirst hist

--Checks if it is safe to play (5,5) so that opponent is unable to score more once you play it
 safe05 ::  Hand  -> DomBoard -> Bool
 safe05 h (Board (x,_) (_,b) hist)
   | ((0,5)`elem` m || (5,0) `elem` m) && ((0,2)`elem` h || (2,0) `elem` h)  = True
   | ((0,2)`elem` m || (2,0) `elem` m) && ((0,5)`elem` h || (5,0) `elem` h)  = True
   | (((0,2) `elem` m) || ((2,0) `elem` m))  && (((0,5) `elem` m) || ((5,0) `elem` m))  = True
   | (((0,2) `elem` h) || ((2,0) `elem` h))  && (((0,5) `elem` h) || ((5,0) `elem` h))  = True
   | otherwise = False
      where m = map extractFirst hist

--Checks if it is safe to play (4,4) so that opponent is unable to score more once you play it
 safe04 ::  Hand  -> DomBoard -> Bool
 safe04 h (Board (x,_) (_,b) hist)
   | ((0,4)`elem` m || (4,0) `elem` m) && ((0,2)`elem` h || (2,0) `elem` h)  = True
   | ((0,2)`elem` m || (2,0) `elem` m) && ((0,4)`elem` h || (4,0) `elem` h)  = True
   | (((0,2) `elem` m) || ((2,0) `elem` m))  && (((0,4) `elem` m) || ((4,0) `elem` m))  = True
   | (((0,2) `elem` h) || ((2,0) `elem` h))  && (((0,4) `elem` h) || ((4,0) `elem` h))  = True
   | otherwise = False
      where m = map extractFirst hist

--Checks if it is safe to play (2,2) so that opponent is unable to score more once you play it
 safe02 ::  Hand  -> DomBoard -> Bool
 safe02 h (Board (x,_) (_,b) hist)
   | ((0,6)`elem` m || (6,0) `elem` m) && ((0,2)`elem` h || (2,0) `elem` h)  = True
   | ((0,2)`elem` m || (2,0) `elem` m) && ((0,6)`elem` h || (6,0) `elem` h)  = True
   | (((0,2) `elem` m) || ((2,0) `elem` m))  && (((0,6) `elem` m) || ((6,0) `elem` m))  = True
   | (((0,2) `elem` h) || ((2,0) `elem` h))  && (((0,6) `elem` h) || ((6,0) `elem` h))  = True
   | otherwise = False
      where m = map extractFirst hist

--Checks if it is safe to play (1,1) so that opponent is unable to score more once you play it
 safe01 ::  Hand  -> DomBoard -> Bool
 safe01 h (Board (x,_) (_,b) hist)
   | ((0,4)`elem` m || (4,0) `elem` m) && ((0,1)`elem` h || (1,0) `elem` h)  = True
   | ((0,1)`elem` m || (1,0) `elem` m) && ((0,4)`elem` h || (4,0) `elem` h)  = True
   | (((0,1) `elem` m) || ((1,0) `elem` m))  && (((0,4) `elem` m) || ((4,0) `elem` m))  = True
   | (((0,1) `elem` h) || ((1,0) `elem` h))  && (((0,4) `elem` h) || ((4,0) `elem` h))  = True
   | otherwise = False
      where m = map extractFirst hist
 
------------------------------------------------------------------------------
-----------------------------------------------------------------------------
{- Checks to see if the most occuring pip in your hand can be played on the board to make
  the onf of theends that majority pip from your hand. It does this only if that majority 
  pip occurs more on the board and in your hand than it could occur in the opponents hand.
 -}

--This function works out if wether or not you have the majority pip of 6 or 5
--There are 7 pips containing number 6 and 5 in them therefore you own the majority
--if 4 or more of those dominoes are either in your hand or on the board
 pipMajorityLength :: Hand ->Hand -> Bool
 pipMajorityLength h z
   | ((findMajorityPip h)==6)  = ((length(findMCDom h) + length(filterBoardHCP z 6)) > 3)
   | ((findMajorityPip h)==5)  = ((length(findMCDom h) + length(filterBoardHCP z 5)) > 3)
   | otherwise = False
  
--Returns true if you have 5 or 6 as a majority pip
 playMajorityPip :: Hand -> Bool
 playMajorityPip h
   | (((findMajorityPip h)==6) || ((findMajorityPip h)==5)) = True
   | otherwise = False

--Extracts the first number out of a triplet. Used for extracting the board from the history
 extractFirst :: (a, b, c) -> a
 extractFirst (a,_,_) = a

--Finds the most common dominoes from a hand and returns the hand filtered with just those dominoes 
 findMCDom :: Hand -> Hand
 findMCDom [] = []
 findMCDom h = 
    let x = findMajorityPip h
    in filter (\n -> ((fst(n)==x) || (snd(n)==x)))  h

--Filters the board for the highest common pip that has been found in your hand
 filterBoardHCP :: Hand -> Int -> Hand
 filterBoardHCP [] _ = []
 filterBoardHCP h x = filter (\n -> ((fst(n)==x) || (snd(n)==x)))  h

--Check if you play the domino with most common pip any of the ends are still your highest pip
 checkEndsHP :: (Dom,End) -> DomBoard -> Int ->  Bool
 checkEndsHP (d,e) z k
   | (x==k) || (b==k) = True
   | otherwise = False
      where (Board (x,_) (_,b) hist) = updateBoard d e P1 z

--Finds the majority pip from your hand, working out mode of fst and snd 
 findMajorityPip :: Hand -> Int
 findMajorityPip h = mode(makePipList (findHand1 h) (findHand2 h))

--Makes a list of your highest pips making sure if you have a double
--it will not count it twice
 makePipList :: [Int] -> [Int]-> [Int]
 makePipList [] [] = []
 makePipList (h:t) (l:r) 
   | (h==l) = h:makePipList t r
   | otherwise = makePipList t r++[h,l]

--Finds out the mode of the fst element of all dominoes
 findHand1 :: Hand -> [Int]
 findHand1 h = map fst h

--Finds out the mode of the snd element of all dominoes
 findHand2 :: Hand -> [Int]
 findHand2 h = map snd h

--Given a list it finds out the most common element of the list
 mode :: Ord a => [a] -> a
 mode = head.maximumBy (comparing length).group.sort
 


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{- Do not play a dangerous dom unless you can knock it off when your turn comes again
 -}
--Finds out if at least an element of a list exists in another list
 exists :: Eq a => [a] -> [a] -> Bool
 exists x y = any id $ (==) <$> x <*> y

--This function given a hand finds out if any element of the hand is one of
--the elements in those lists
 anyPipInHand :: Hand -> Bool
 anyPipInHand h =
   let a = [(6,5),(6,4),(6,3),(6,2),(6,1),(6,0)]
       b = [(5,4),(5,3),(5,2),(5,1),(5,0),(6,5)]
       c = [(4,3),(4,2),(4,1),(4,0),(5,4),(6,4)]
       d = [(3,2),(3,1),(3,0),(4,3),(5,3),(6,3)]
       e = [(2,1),(2,0),(3,2),(4,2),(5,2),(6,2)]
       f = [(1,0),(2,1),(3,1),(4,1),(5,1),(6,1)]

   in (exists h a || exists h b || exists h c || exists h d || exists h e || exists h f)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------     

 -- hsdplayer plays highest scoring dom
 -- we have  hsd :: Hand->DomBoard->(Dom,End,Int)
 
 hsdPlayer h b p s = (d,e)
                     where (d,e,_)=hsd h b
                     
  -- highest scoring dom

 hsd :: Hand->DomBoard->(Dom,End,Int)
 
 hsd h InitBoard = (md,L,ms)
  where
   dscores = zip h (map (\ (d1,d2)->score53 (d1+d2)) h)
   (md,ms) = maximumBy (comparing snd) dscores
   
 
 hsd h b = 
   let
    ld =  leftdrops h b
    rd = rightdrops h b
    lscores = zip ld (map (\d->(scoreDom d L b)) ld) -- [(Dom, score)]
    rscores = zip rd (map (\d->(scoreDom d R b)) rd)
    (lb,ls) = if (not(null lscores)) then (maximumBy (comparing snd) lscores) else ((0,0),-1) -- can't be chosen
    (rb,rs) = if (not(null rscores)) then (maximumBy (comparing snd) rscores) else ((0,0),-1)
   in
    if (ls>rs) then (lb,L,ls) else (rb,R,rs)
 
 
                                               
 -----------------------------------------------------------------------------------------
 {- top level fn
    args: 2 players (p1, p2), number of games (n), random number seed (seed)
    returns: number of games won by player 1 & player 2
    calls playDomsGames giving it n, initial score in games and random no gen
 -} 
 
 domsMatch :: DomsPlayer->DomsPlayer->Int->Int->(Int,Int)
 
 domsMatch p1 p2 n seed = playDomsGames p1 p2 n (0,0) (mkStdGen seed)
 
 -----------------------------------------------------------------------------------------
 
{- playDomsGames plays n games

  p1,p2 players
  (s1,s2) their scores
  gen random generator
-}
 
 playDomsGames :: DomsPlayer->DomsPlayer->Int->(Int,Int)->StdGen->(Int,Int)
 
 playDomsGames _ _ 0 score_in_games _ = score_in_games -- stop when n games played
 
 playDomsGames p1 p2 n (s1,s2) gen 
   |gameres==P1 = playDomsGames p1 p2 (n-1) (s1+1,s2) gen2 -- p1 won
   |otherwise = playDomsGames p1 p2 (n-1) (s1,s2+1) gen2 -- p2 won
  where
   (gen1,gen2)=split gen -- get 2 generators, so doms can be reshuffled for next hand
   gameres = playDomsGame p1 p2 (if (odd n) then P1 else P2) (0,0) gen1 -- play next game p1 drops if n odd else p2
 
 -----------------------------------------------------------------------------------------
 -- playDomsGame plays a single game - 61 up
 -- returns winner - P1 or P2
 -- the Bool pdrop is true if it's p1 to drop
 -- pdrop alternates between games
 
 playDomsGame :: DomsPlayer->DomsPlayer->Player->(Int,Int)->StdGen->Player
 
 playDomsGame p1 p2 pdrop scores gen 
  |s1==61 = P1
  |s2==61 = P2
  |otherwise = playDomsGame p1 p2 (if (pdrop==P1) then P2 else P1) (s1,s2) gen2
  where
   (gen1,gen2)=split gen
   (s1,s2)=playDomsHand p1 p2 pdrop scores gen1  
  
 -----------------------------------------------------------------------------------------
 -- play a single hand
  
 playDomsHand :: DomsPlayer->DomsPlayer->Player->(Int,Int)->StdGen->(Int,Int)
 
 playDomsHand p1 p2 nextplayer scores gen = 
   playDoms p1 p2 init_gamestate
  where
   spack = shuffleDoms gen
   p1_hand = take 9 spack
   p2_hand = take 9 (drop 9 spack)
   init_gamestate = (p1_hand,p2_hand,nextplayer,InitBoard,scores) 
   
 ------------------------------------------------------------------------------------------   
 -- shuffle 
 
 shuffleDoms :: StdGen -> [Dom]

 shuffleDoms gen =  
  let
    weights = take 28 (randoms gen :: [Int])
    dset = (map fst (sortBy  
               (\ (_,w1)(_,w2)  -> (compare w1 w2)) 
               (zip domSet weights)))
  in
   dset
   
 ------------------------------------------------------------------------------------------
 -- playDoms runs the hand
 -- returns scores at the end

 
 playDoms :: DomsPlayer->DomsPlayer->GameState->(Int,Int)
 
 playDoms _ _ (_,_,_,_, (61,s2)) = (61,s2) --p1 has won the game
 playDoms _ _ (_,_,_,_, (s1,61)) = (s1,61) --p2 has won the game
 
 
 playDoms p1 p2 gs@(h1,h2,nextplayer,b,scores)
  |(kp1 &&  kp2) = scores -- both players knocking, end of the hand
  |((nextplayer==P1) && (not kp1)) =  playDoms p1 p2 (p1play p1 gs) -- p1 plays, returning new gameState. p2 to go next
  |(nextplayer==P1) = playDoms p1 p2 (p2play p2 gs) -- p1 knocking so p2 plays
  |(not kp2) = playDoms p1 p2 (p2play p2 gs) -- p2 plays
  |otherwise = playDoms p1 p2 (p1play p1 gs) -- p2 knocking so p1 plays
  where
   kp1 = knocking h1 b -- true if p1 knocking
   kp2 = knocking h2 b -- true if p2 knocking
   
 ------------------------------------------------------------------------------------------
 -- is a player knocking?

 knocking :: Hand->DomBoard->Bool
 
 knocking h b = 
  ((null (leftdrops h b)) && (null (rightdrops h b))) -- leftdrops & rightdrops in doms.hs
 
 ------------------------------------------------------------------------------------------
   
 -- player p1 to drop
 
 p1play :: DomsPlayer->GameState->GameState
 
 p1play p1 (h1,h2,_,b, (s1,s2)) = 
  ((delete dom h1), h2, P2,(updateBoard dom end P1 b), (ns1, s2))
   where
    (dom,end) = p1 h1 b P1 (s1,s2)-- call the player, returning dom dropped and end it's dropped at.
    score = s1+ (scoreDom dom end b) -- what it scored
    ns1 = if (score >61) then s1 else score -- check for going bust
    
 
 -- p2 to drop
   
 p2play :: DomsPlayer->GameState->GameState
 
 p2play p2 (h1,h2,_,b,(s1,s2)) = 
  (h1, (delete dom h2),P1, (updateBoard dom end P2 b), (s1, ns2))
  where
   (dom,end) = p2 h2 b P2 (s1,s2)-- call the player, returning dom dropped and end it's dropped at.
   score = s2+ (scoreDom dom end b) -- what it scored
   ns2 = if (score >61) then s2 else score -- check for going bust
 
   -------------------------------------------------------------------------------------------
 -- updateBoard 
 -- update the board after a play
 
 updateBoard :: Dom->End->Player->DomBoard->DomBoard
 
 updateBoard d e p b
  |e==L = playleft p d b
  |otherwise = playright p d b

  ------------------------------------------------------------------------------------------
 -- doms which will go left
 leftdrops :: Hand->DomBoard->Hand
 
 leftdrops h b = filter (\d -> goesLP d b) h
 
 -- doms which go right
 rightdrops :: Hand->DomBoard->Hand
 
 rightdrops h b = filter (\d -> goesRP d b) h 
 
 -------------------------------------------------
 -- 5s and 3s score for a number
  
 score53 :: Int->Int
 score53 n = 
  let 
   s3 = if (rem n 3)==0 then (quot n 3) else 0
   s5 = if (rem n 5)==0 then (quot n 5) else 0 
  in
   s3+s5
   
 ------------------------------------------------ 
 -- need comparing
 -- useful fn specifying what we want to compare by
 comparing :: Ord b=>(a->b)->a->a->Ordering
 comparing f l r = compare (f l) (f r)
 
 ------------------------------------------------
 -- scoreDom
 -- what will a given Dom score at a given end?
 -- assuming it goes
 
 scoreDom :: Dom->End->DomBoard->Int
 
 scoreDom d e b = scoreboard nb
                  where
                  (Just nb) = (playDom P1 d e b) -- player doesn't matter
 
 ----------------------------------------------------                 
 -- play to left - it will go
 playleft :: Player->Dom->DomBoard->DomBoard
 
 playleft p (d1,d2) InitBoard = Board (d1,d2) (d1,d2) [((d1,d2),p,1)]
 
 playleft p (d1,d2) (Board (l1,l2) r h)
  |d1==l1 = Board (d2,d1) r (((d2,d1),p,n+1):h)
  |otherwise =Board (d1,d2) r (((d1,d2),p,n+1):h)
  where
    n = maximum [m |(_,_,m)<-h] -- next drop number
    
 -- play to right
 playright :: Player->Dom->DomBoard->DomBoard
 
 playright p (d1,d2) InitBoard = Board (d1,d2) (d1,d2) [((d1,d2),p,1)]
 
 playright p (d1,d2)(Board l (r1,r2) h)
  |d1==r2 = Board l (d1,d2) (h++[((d1,d2),p,n+1)])
  |otherwise = Board l (d2,d1) (h++[((d2,d1),p,n+1)])
  where 
    n = maximum [m |(_,_,m)<-h] -- next drop number
 
 ------------------------------------------------------
 -- predicate - will given domino go at left?
 -- assumes a dom has been played
 
 goesLP :: Dom->DomBoard->Bool
 
 goesLP _ InitBoard = True
 
 goesLP (d1,d2) (Board (l,_) _ _) = (l==d1)||(l==d2)


 -- will dom go to the right?
 -- assumes a dom has been played
 
 goesRP :: Dom->DomBoard->Bool
 
 goesRP _ InitBoard = True
 
 goesRP (d1,d2) (Board _ (_,r) _) = (r==d1)||(r==d2)
 
 ------------------------------------------------

 -- playDom
 -- given player plays
 -- play a dom at left or right, if it will go

 
 playDom :: Player->Dom->End->DomBoard->Maybe DomBoard
 
 playDom p d L b
   |goesLP d b = Just (playleft p d b)
   |otherwise = Nothing
 
 playDom p d R b
   |goesRP d b = Just (playright p d b)
   |otherwise = Nothing
   
 ---------------------------------------------------    
 -- 5s & threes score for a board
 scoreboard :: DomBoard -> Int
 scoreboard InitBoard = 0
 scoreboard (Board (l1,l2) (r1,r2) hist)
  |length hist == 1 = score53 (l1+l2) -- 1 dom played, it's both left and right end
  |otherwise = score53 ((if l1==l2 then 2*l1 else l1)+ (if r1==r2 then 2*r2 else r2))
 
--Filters the hand removing a particular domino from a particular hand     
 filterHand :: Hand -> Dom ->  Hand
 filterHand h x = filter ( \n -> n /=x  ) h