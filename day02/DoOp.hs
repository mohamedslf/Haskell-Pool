--
-- EPITECH PROJECT, 2021
-- DoOp
-- File description:
-- all Doop functions
--

import Data.Char
import Control.Monad

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs)
  | a == x = True
  | otherwise = myElem a xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = Just $div a b

safeNth :: [a] -> Int -> Maybe a
safeNth (x : _) 0 = Just x
safeNth [] _ = Nothing
safeNth (_ : xs) y
  | y < 0 = Nothing
  | y > length xs = Nothing
  | otherwise = safeNth xs (y - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just x) = Just (x + 1)

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup a ((x, b) : ys)
  | a == x = Just b
  | otherwise = myLookup a ys

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ Nothing _ = Nothing
maybeDo f a b = Just f <*> a <*> b

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt ('-':str)
  | all isDigit str = Just (read ('-':str) :: Int)
  | otherwise = Nothing
readInt str
  | all isDigit str = Just (read str :: Int)
  | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do
  str <- getLine
  return (length str)

printAndGetLength :: String -> IO Int
printAndGetLength  str = putStrLn str >> return(length str)

getInt :: IO (Maybe Int)
getInt = do
  nbr <- getLine
  let x = read nbr :: Int
  return (Just x)

occurence :: Int -> Char -> IO ()
occurence 0 c = return ()
occurence n c = putChar c >>
          occurence (n - 1) c

printPip :: Int -> IO ()
printPip 0 = return ()
printPip n = putChar '|' >>
          occurence (n * 2 - 2) ' ' >>
          putStrLn "|"

bodyBuild :: Int -> IO ()
bodyBuild n = putStr "+" >>
            occurence (n * 2 - 2) '-' >>
            putStrLn "+"

printBox :: Int -> IO ()
printBox 0 = return ()
printBox 1 = putStrLn "++"
printBox n | n < 0 = return ()
           | otherwise =
             bodyBuild n >>
             replicateM_ (n - 2) (printPip n) >>
             bodyBuild n

concatLines :: Int -> IO String
concatLines n = do
  str <- replicateM n getLine
  return $ concat str

main :: IO ()
main = printBox 20
