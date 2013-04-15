{-# LANGUAGE UnicodeSyntax #-}
import System.Random(randomRIO)
import Data.Char (digitToInt)

----------------------------------------------------------------------------------------------------prompting user
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
-------------------------------------------------------------------------------------------------------------end of prompting user

-------------------------------------------------------------------------------------------------------------changing representations
convertAtoms :: Int -> String
convertAtoms atomNum = 
    take atomNum ['A'..'Z']


convertOperators :: Int -> String
convertOperators 1 = "∧"
convertOperators 2 = "∨"
convertOperators 3 = "→"
--------------------------------------------------------------------------------------------------------------end of changing representations

--------------------------------------------------------------------------------------------------------------start of making argument

getConclusion :: [String] -> String -> String
getConclusion operators atomNum = undefined


makePremise :: String -> [String] -> Int -> String -> IO String
makePremise = undefined


makeArgument :: [String] -> (Int,Int) -> (Int,Int) -> IO String
makeArgument operators premRange atomRange =
   do
    premNum <- randomRIO premRange
    atomNum <- randomRIO atomRange
    let atoms = convertAtoms atomNum
    let conclusion = getConclusion operators atoms
    makePremise conclusion operators premNum atoms

--------------------------------------------------------------------------------------------------------------end of making argument




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
    putStrLn "Thank You!\nExecuting..."
    putStrLn "Good Bye"