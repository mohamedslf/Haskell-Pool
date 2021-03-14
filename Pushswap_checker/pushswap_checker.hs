--
-- EPITECH PROJECT, 2021
-- B-FUN-300-COT-3-1-pushswapchecker-mohamed.salifou
-- File description:
-- PC
--

import Data.List
import Data.Char
import System.Environment
import System.Exit

sa :: ([l_a], [l_b]) -> ([l_a], [l_b])
sa ([], lister) = ([], lister)
sa (x : xs, lister) = (y : z : ys, lister)
  where
    z = x
    last = drop 1 (x : xs)
    (y : ys) = last

sb :: ([l_a], [l_b]) -> ([l_a], [l_b])
sb (lister, []) = (lister, [])
sb (lister, x : xs) = (lister, y : z : ys)
  where
    z = x
    last = drop 1 (x : xs)
    (y : ys) = last

sc :: ([l_a], [l_c]) -> ([l_a], [l_c])
sc a = sb (sa a)

pa :: ([b], [b]) -> ([b], [b])
pa (a, []) = (a, [])
pa (a, x : xs) = (x:a, xs)

pb :: ([a], [a]) -> ([a], [a])
pb ([], b) = ([], b)
pb (x : b, xs) = (b, x : xs)

ra :: ([l_c], [l_a]) -> ([l_c], [l_a])
ra (x, []) = (x, [])
ra ([], y) = ([], y)
ra (la, l) = (tail la ++ [head la], l)

rb :: ([l_c], [l_a]) -> ([l_c], [l_a])
rb ([], x) = ([], x)
rb (y, []) = (y, [])
rb (l, la) = (l, tail la ++ [head la])

rr :: ([l_c], [l]) -> ([l_c], [l])
rr a = rb (ra a)

rra :: ([l_c], [list]) -> ([l_c], [list])
rra (a, l) = (drop (length a -1) a ++ take (length a - 1) a, l)

rrb :: ([l_c], [list]) -> ([l_c], [list])
rrb (l, b) = (l, drop (length b -1) b ++ take (length b - 1) b)

rrr :: ([l_c], [list]) -> ([l_c], [list])
rrr a = rra (rrb a)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt ('-':str)
  | all isDigit str = Just (read ('-':str) :: Int)
  | otherwise = Nothing
readInt str
  | all isDigit str = Just (read str :: Int)
  | otherwise = Nothing

checkReadInt :: [String] -> IO ()
checkReadInt [] = return ()
checkReadInt (x:xs) | readInt x == Nothing = exitWith (ExitFailure 84)
             | otherwise = checkReadInt (xs)

callOperationf :: ([String], [String]) -> String -> ([String], [String])
callOperationf (f, s) "sa" = sa (f, s)
callOperationf (f, s) "sb" = sb (f, s)
callOperationf (f, s) "sc" = sc (f, s)
callOperationf (f, s) "pa" = pa (f, s)
callOperationf (f, s) "pb" = pb (f, s)

callOperations :: ([String], [String]) -> String -> ([String], [String])
callOperations (f, s) "ra" = ra (f, s)
callOperations (f, s) "rb" = rb (f, s)
callOperations (f, s) "rr" = rr (f, s)
callOperations (f, s) "rra" = rra (f, s)
callOperations (f, s) "rrb" = rrb (f, s)
callOperations (f, s) "rrr" = rrr (f, s)

callOperation :: ([String], [String]) -> String -> ([String], [String])
callOperation (f, s) cmd | cmd == "sa" || cmd == "sb" || cmd == "sc" ||
                           cmd == "pa" || cmd == "pb" =
                           callOperationf (f, s) cmd
                         | otherwise = callOperations (f, s) cmd

doOperation :: ([String], [String]) -> [String] -> [String]
doOperation (la, _) [] = la
doOperation (la, lb) (z:zs) = doOperation (callOperation (la, lb) z) zs

isSorted :: ([String], [String]) ->[String] -> Bool
isSorted (la, lb) str | sort la == doOperation (la, lb) str = True
                      | otherwise = False

printMessage :: ([String], [String]) ->[String] -> IO ()
printMessage (la, lb) str | isSorted (la, lb) str == True = putStrLn "OK"
                          | otherwise = putStrLn "KO"

main :: IO ()
main = do
  operations <- getLine
  args <- getArgs
  let ops = words operations
  checkReadInt args
  printMessage (args, []) ops
  exitSuccess