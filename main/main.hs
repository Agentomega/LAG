-- Logical Argument Generator
-- Author: Joe Allen && Joe Valerio
-- Computability and Logic: Final project


{-# LANGUAGE UnicodeSyntax #-}
import System.Random(randomRIO)
import Data.Random.Extras(choice)
import Data.Random.RVar(runRVar)
import Data.Random
import Data.Char (digitToInt)
import Data.List(elemIndices, splitAt )

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
convertAtoms :: Int -> [String]
convertAtoms atomNum = 
    let posConclusion = map (:[]) (take atomNum ['A'..'Z'])
    in posConclusion ++ map ('-':) posConclusion



convertOperators :: Int -> Char
convertOperators 1 = '∧'
convertOperators 2 = '∨'
convertOperators 3 = '→'


--------------------------------------------------------------------------------------------------------------end of changing representations

--------------------------------------------------------------------------------------------------------------start of making argument
checkAtoms :: String -> String -> Bool
checkAtoms atom1 atom2 = 
   not (atom1 /= "$" && atom2 /= "$") || (atom1 /= atom2)


expandDol :: String -> String -> [String] -> IO String
expandDol conclusion operators atoms = do
    let (ys,zs) = splitAt (head (take 1 replacement)) conclusion
    conclusion' <- getConclusion operators atoms
    if length replacement > 1 then expandDol (ys ++ conclusion' ++ tail zs) operators atoms
     else return (ys ++ conclusion' ++ tail zs)
  where replacement = elemIndices '$' conclusion



getConclusion :: String -> [String] -> IO String
getConclusion operators atoms = do
   conclusion <- runRVar (choice ["(" ++ atom1 ++ " " ++ [operator] ++ " " ++ atom2 ++ ")"| atom1 <- atoms, atom2 <- atoms, operator <- operators, checkAtoms atom1 atom2]) StdRandom
   if '$' `elem` conclusion then expandDol conclusion operators atoms
    else return conclusion


makePremise :: IO String -> String -> Int -> [String] -> IO String
makePremise = undefined


makeArgument :: String -> (Int,Int) -> (Int,Int) -> IO String
makeArgument operators premRange atomRange =
   do
    premNum <- randomRIO premRange
    atomNum <- randomRIO atomRange
    let atoms = convertAtoms atomNum ++ ["$"]
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
    makeArgument operators premRange atomRange
    putStrLn "Thank You!\nExecuting..."
    putStrLn "Good Bye"