{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative
import HareMonad

data RE :: * -> * where
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a,b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char cs) = do
  x <- readCharacter
  guard (x `elem` cs)
  pure x
match (Seq a b) = do
  ra <- match a
  rb <- match b
  pure ((,) ra rb)
match (Choose a b) = do
  match a <|> match b
match (Star a) =
        addFront <$> match a <*> match (Star a)
    <|> pure []
  where
    addFront x xs = (x:xs)
match (Action f a ) = f <$> match a

matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a
(=~) = flip (hare . matchAnywhere)

infixr `cons`
cons :: RE a -> RE [a] -> RE [a]
cons a as = Action (\(x,y) -> x:y) (Seq a as)

string :: String -> RE String
string [] = Action (\a -> []) Empty
string (a:as) = cons (Char [a]) (string as)

rpt :: Int -> RE a -> RE [a]
rpt n a  | n < 1 = Action (\a -> []) Empty
         | otherwise = cons a (rpt (n-1) a)

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) a =  choose (map (\x -> rpt x a) n)
                      where n = [y,y-1..x]

option :: RE a -> RE (Maybe a)
option a = Choose (Action (\x -> Nothing) Empty) (Action (\x -> Just x) a)

plus :: RE a -> RE [a]
plus a =  cons a (Star a)

choose :: [RE a] -> RE a
choose [] = Fail
choose (a:as) = Choose a (choose as)
