

import Data.Char (digitToInt, isDigit, toUpper)

-- Data types
data Color = Red | Black deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq)
data Rank = Num Int | Jack | Queen | King | Ace deriving (Show, Eq)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Show, Eq)
data Move = Draw | Discard Card deriving (Show, Eq)


cardColor :: Card -> Color

cardColor (Card suit _) = if suit == Spades || suit == Clubs then Black else Red


cardValue :: Rank -> Int
cardValue rank =
    case rank of
        Num 2 -> 2
        Num 3 -> 3
        Num 4 -> 4
        Num 5 -> 5
        Num 6 -> 6
        Num 7 -> 7
        Num 8 -> 8
        Num 9 -> 9
        Jack -> 10
        Queen -> 10
        King -> 10
        Ace -> 11
        _ -> error "Bitte ein gültige Zahl verwenden    "


convertSuit :: Char->Suit
convertSuit c = 
    case toUpper c of
        'D'-> Diamonds
        'C' -> Clubs
        'H' -> Hearts
        'S' -> Spades
        _ ->  error "Try again"



convertRank :: Char -> Rank
convertRank c =
    if isDigit c
        then case c of
            '1' -> Num 1
            '2' -> Num 2
            '3' -> Num 3
            '4' -> Num 4
            '5' -> Num 5
            '6' -> Num 6
            '7' -> Num 7
            '8' -> Num 8
            '9' -> Num 9
            _ -> error "Invalid digit"
        else
            case toUpper c of
                'T' -> Num 10
                'Q' -> Queen
                'K' -> King
                'J' -> Jack
                'A' -> Ace
                _ -> error "Invalid rank"


convertCard :: Char->Char->Card

convertCard s r = Card {suit = convertSuit s,rank =convertRank r}

convertMove :: Char->Char->Char->Move

convertMove m s r 
  | toUpper m  == 'D' = Draw
  | toUpper m  == 'R' = Discard (Card { suit = convertSuit s, rank =convertRank r})
  | otherwise = error "Use different Move"



removeCard :: [Card]->Card->[Card]
removeCard [] _ = []
removeCard (x:xs) c 
 |c==x = xs
 |otherwise = x: removeCard xs c



allSameColor :: [Card] -> Bool
allSameColor([]) = True
allSameColor (_:[]) = True
allSameColor (x:y:xs) 
  | cardColor x == cardColor y = allSameColor (y:xs)
  | otherwise = False
allSameColor _ = False

sumCards :: [Card] -> Int
sumCards [] = 0
sumCards (x:xs) = cardValue (rank x) + sumCards xs

sumCardss :: [Card]->Int
sumCardss cards = sum [cardValue(rank card)| card <-cards]

 
score2 :: [Card]->Int->Int
score2 cards goal 
 |sumCards (cards) > goal = 3 * ((sumCards cards) - goal)
 |otherwise = goal - sumCards cards

score1 :: [Card]->Int->Int
score1 cards goal 
 | allSameColor cards  = score2 cards (goal `div` 2)
 | otherwise = score2 cards goal

 

addHeldCard :: [Card] -> Card -> [Card]
addHeldCard xs card = xs ++ [card]

subheldcards :: [Card] -> Card -> [Card]
subheldcards xs card = [x | x <- xs, x /= card]

rrunGame :: [Card] -> [Move] -> Int -> Int

rrunGame (y:ys) (x:xs) goal = rrunGame' (y:ys) (x:xs) goal []  -- Starten Sie mit einer leeren Liste für heldcards.

rrunGame' :: [Card] -> [Move] -> Int -> [Card] -> Int
rrunGame' [] _ goal heldcards = score1 heldcards goal  
rrunGame' _ [] goal heldcards = score1 heldcards goal  -- Basisfall: Wenn die Karten erschöpft sind, berechnen Sie den endgültigen Score.
rrunGame' (y:ys) (x:xs) goal heldcards
  | sumCards heldcards > goal = score1 heldcards goal 
  | otherwise =
      case x of
        Draw -> rrunGame' ys xs goal (addHeldCard heldcards y)  -- Aktualisieren Sie heldcards, indem Sie die gezogene Karte hinzufügen.
                       
        Discard card -> rrunGame' (y:ys) xs goal (subheldcards heldcards card)  -- Aktualisieren Sie heldcards, indem Sie die verworfene Karte entfernen.

        _ -> error "CARD IS NOT IN YOUR Hand"
-- Beispielkartenr
card1 = Card { suit = Spades, rank = Num 4 }
card2 :: Card
card2 = Card { suit = Hearts, rank = Queen }
card3 = Card { suit = Diamonds, rank = Num 7 }
card4 = Card { suit = Clubs, rank = Ace }

-- Beispiel-Moves
move1 = Draw

-- Spielsituation
move = [move1]
deck = [card3, card4,card3]





