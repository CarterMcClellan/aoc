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

-- Functions Needed for part 2

-- General strat, we want to iterate character by character
--      if one of the words which we are looking for is a prefix of the current string;
--         then replace the current string with the digit
--      else; move forward one character
convertWordsToDigits :: String -> String
convertWordsToDigits str = convertWordsToDigitsHelper str wordToDigitMappings
  where
    wordToDigitMappings = [("one", '1'), ("two", '2'), ("three", '3'), 
                           ("four", '4'), ("five", '5'), ("six", '6'), 
                           ("seven", '7'), ("eight", '8'), ("nine", '9')]

convertWordsToDigitsHelper :: String -> [(String, Char)] -> String
convertWordsToDigitsHelper [] _ = [] -- base case
convertWordsToDigitsHelper str mappings = 
    case findMapping str mappings of
        -- if digit is found, prepend as digit
        Just digit -> digit : convertWordsToDigitsHelper (dropWord str mappings) mappings
        -- if digit not found, do nothing
        Nothing -> head str : convertWordsToDigitsHelper (tail str) mappings
  where
    -- Takes the string + mappings/ return the first digit found
    findMapping :: String -> [(String, Char)] -> Maybe Char
    findMapping s mappings = fmap snd (find (isWordPrefix s) mappings)

    -- Remove the word from the beginning of the string
    dropWord :: String -> [(String, Char)] -> String
    dropWord s mappings = 
        case find (isWordPrefix s) mappings of
            Just (word, _) -> 
                drop (length word - 1) s
            Nothing -> s

    -- Checks if a word from the mappings is a prefix of the given string
    isWordPrefix :: String -> (String, Char) -> Bool
    isWordPrefix s (word, _) = isPrefixOf word s