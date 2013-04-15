{-# LANGUAGE UnicodeSyntax #-}
import System.Random(randomRIO)
import Data.Char (digitToInt)

----------------------------------------------------------------------------------------------------start of IO functions
getArguments :: IO Int
getArguments =
    do putStrLn "Enter the number of arguments you want to have"
       arguments <- getChar
       return (digitToInt arguments)

getValidity :: IO Bool
getValidity =
    do putStrLn "Please enter \n(1)for valid\n(2) for invalid"
       prevalidity <- getChar
       return (digitToInt prevalidity == 1)

getRange :: IO (Int,Int)
getRange = 
       do
        putStrLn "Enter lower bound"
        prelower <- getChar
        let lower = digitToInt prelower
        putStrLn "Enter upper bound"
        preupper <- getChar
        let upper = digitToInt preupper
        return (lower,upper)

getOperators :: IO [Int]
getOperators = 
        do 
         putStrLn "Enter the operators you want\n(1) for (∧)\n(2) for (∨)\n(3) for (→)\n no spaces or commas\n"
         operators <- getLine
         return (map digitToInt operators)
-------------------------------------------------------------------------------------------------------------end of IO
convertOperators :: Int -> String
convertOperators 1 = "∧"
convertOperators 2 = "∨"
convertOperators 3 = "→"

getConclusion :: [Int] -> Int -> String
getConclusion operators atomNum = undefined


makePremise :: String -> [Int] -> Int -> Int -> IO String
makePremise = undefined


makeArgument :: [Int] -> (Int,Int) -> (Int,Int) -> IO String
makeArgument operators premRange atomRange =
   do
    premNum <- randomRIO premRange
    atomNum <- randomRIO atomRange
    let conclusion = getConclusion operators atomNum
    makePremise conclusion operators premNum atomNum




main :: IO()
main =
    do   
    putStrLn "Welcome to Logical Argument Generator"
    arguments <- getArguments
    validity <- getValidity
    putStrLn "Enter the range of the number of premises to each argument"
    premRange <- getRange
    putStrLn "Enter the range of the number of atomic statments per premises"
    atomRange <- getRange
    dummyOperators <- getOperators
    let operators = map convertOperators dummyOperators
    print operators
    putStrLn "Thank You!\nExecuting..."
    putStrLn "Good Bye"