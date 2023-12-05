import System.IO
import Data.Char (isDigit)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

main :: IO ()
main = do
    handle <- openFile "inputs/1.txt" ReadMode
    total <- processFile handle 0
    print total -- 54968
    hClose handle

    handle <- openFile "inputs/1.txt" ReadMode
    total2 <- processFile2 handle 0
    print total2 -- 50094
    hClose handle

-- ignore all alphabetical
processFile :: Handle -> Int -> IO Int
processFile handle sumSoFar = do
    eof <- hIsEOF handle
    if eof
        then return sumSoFar
        else do
            line <- hGetLine handle
            let digits = filter isDigit line
            let concatenatedNum = calculateConcatenatedNum digits
            processFile handle (sumSoFar + concatenatedNum)

-- convert specific sequences ('one', 'two', etc... into their digit counterpart, 
-- then sum)
processFile2 :: Handle -> Int -> IO Int
processFile2 handle sumSoFar = do
    eof <- hIsEOF handle
    if eof
        then return sumSoFar
        else do
            line <- hGetLine handle
            let convertedLine = convertWordsToDigits line
            let digits = filter isDigit convertedLine
            let concatenatedNum = calculateConcatenatedNum digits
            -- print (line ++ "|" ++ convertedLine ++ "|" ++ show concatenatedNum)
            processFile2 handle (sumSoFar + concatenatedNum)

calculateConcatenatedNum :: String -> Int
calculateConcatenatedNum digits = 
    if length digits > 1 
    -- head, last are operators on a list
    -- read -> casts all to an int
    then read [head digits, last digits] :: Int
    -- cycle -> converts it to an infinitely 
    -- long list of the singleton digit

    -- take 2 puls the first two digits off the list
    else read (take 2 $ cycle digits) :: Int

-- General strat, replace word with new word then do same as before
convertWordsToDigits :: String -> String
convertWordsToDigits str = foldl replace str wordToDigitMappings 
  where
    wordToDigitMappings = [("one", "o1e"), ("two", "t2o"), ("three", "t3e"), 
                           ("four", "f4r"), ("five", "f5e"), ("six", "s6x"), 
                           ("seven", "s7n"), ("eight", "e8t"), ("nine", "n9e")]

-- Replace function: Replaces all occurrences of a word in a string with another
replace :: String -> (String, String) -> String
replace s (old, new) = replace' s
  where
    replace' "" = ""
    replace' str
      | old `isPrefixOf` str = new ++ replace' (drop (length old) str)
      | otherwise = head str : replace' (tail str) -- otherwise => else guard