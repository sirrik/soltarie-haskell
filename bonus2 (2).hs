{-

Sırrı Kırımlıoğlu
150130117

-}
data Color = Red 
			| Black
			deriving(Eq,Show)

data Suit = Clubs
			| Diamonds
			| Hearts
			| Spades
			deriving(Eq,Show)

data Rank = Two -- Num Int doesnt work as card1 = Card Clubs 4 so i changed Num Int with numbers writings
			| Three 
			| Four
			| Five
			| Six
			| Seven
			| Eight
			| Nine
			| Jack
			| Queen
			| King
			| Ace
			deriving(Eq,Show)

data Card = Card { suit :: Suit, rank :: Rank }
			deriving(Eq,Show)

data Move = Draw
			| Discard Card
			deriving(Eq,Show)

data State = Continues  --There are remaining moves and sum of values is not greater than the goal
			| Ended -- No more moves or sum of values is greater than the goal
			deriving(Eq,Show)

heldcards = []

cardColor :: Card -> Color
cardColor (Card suit _) | suit == Hearts || suit == Diamonds = Red
                     	| otherwise = Black

cardValue :: Card -> Integer
cardValue (Card _ rank) | rank == Jack || rank == Queen || rank == King = 10
            			| rank == Ace = 11
          				| rank == Two = 2
    			        | rank == Three = 3
			            | rank == Four = 4
            			| rank == Five = 5
            			| rank == Six = 6
			            | rank == Seven = 7
            			| rank == Eight = 8
            			| rank == Nine = 9
            			| otherwise = error "Invalid Card"

removeCard :: [Card] -> Card -> [Card]
removeCard [] _					= []
removeCard (c:cs) x | x == c	= cs
					| otherwise = c : removeCard cs x


allSameColor :: [Card] -> Bool
allSameColor []												= True
allSameColor [c] 											= True
allSameColor (c:cs) | cardColor c == cardColor (head cs)	= allSameColor cs
					| otherwise 							= False

sumCard :: [Card] -> Integer
sumCard []		= 0
sumCard (c:cs) 	= sumer cs (cardValue c)
				where
					sumer :: [Card] -> Integer -> Integer
					sumer [] sum		 = sum
					sumer (c:cs) sum = sumer cs (sum + cardValue c)

score :: [Card] -> Integer -> Integer
score [] goal   = error "No cards"
score cs goal | sumCard cs > goal  && allSameColor cs == False = 3 * (sumCard cs - goal)
			  | sumCard cs > goal  && allSameColor cs == True  = (3 * (sumCard cs - goal)) `div` 2
			  | sumCard cs <= goal && allSameColor cs == False = goal - sumCard cs
			  | otherwise = (goal - sumCard cs) `div` 2 

runGame :: [Card] -> [Move] -> Integer
runGame cs [] goal = gameState Ended
runGame cs ms goal = gameState Continues
					where
						gameState :: State -> Integer
						gameState Ended = score heldcards goal
						gameState Continues = makemove cs ms heldcards
							where 
								makemove :: [Card] -> [Move] -> [Card] -> State
								makemove cs [] heldcards = gameState Ended
								makemove [] ms heldcards | m == Draw = gameState Ended
								makemove (c:cs) (m:ms) heldcards | sumCard heldcards > goal = gameState Ended
																 | m == Discard x = removeCard heldcards x && gameState Continues 
													   			 | m == Draw = c : heldcards && gameState Continues
