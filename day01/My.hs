--
-- EPITECH PROJECT, 2021
-- my
-- File description:
-- all the haskell functions
--


mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x | x < 0 = True
          | otherwise = False

myAbs :: Int -> Int
myAbs x | myIsNeg x = -x
        | otherwise = x

myMin :: Int -> Int -> Int
myMin x y | x < y = x
          | otherwise = y

myMax :: Int -> Int -> Int
myMax x y | x < y = y
          | otherwise = x

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead [] = error"ERROR"
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail [] = error"ERROR"
myTail (x:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = myLength xs + 1

myNth :: [a] -> Int -> a
myNth [] _ = error"ERROR"
myNth (x:xs) 0 = x
myNth (x:xs) y | y < 0 = error"invalid position"
               | y > myLength (x:xs) = error"too large"
               | otherwise = myNth xs (y - 1)

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _  = []
myTake y (x:xs) = x: myTake (y-1) xs

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop x xs | x <= 0 = xs
myDrop y (_:xs) = myDrop (y - 1) xs

myInit :: [a] -> [a]
myInit [] = error"ERROR"
myInit [a] = [a]
myInit (x:xs) = x : init xs

myLast :: [a] -> a
myLast [] = error"ERROR"
myLast (x:xs) = x

myMap :: (a -> b) -> [a] -> [b]
myMap y [] = []
myMap y (x:xs) = y x : myMap y xs

myAppend :: [a] -> [a] -> [a]
myAppend [] x = x
myAppend (x:xs) y = x : myAppend xs y
