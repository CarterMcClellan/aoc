import System.IO
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.List (groupBy)
import Data.Char (isSpace)
import Debug.Trace (trace)

main :: IO ()
main = do 
    -- handle <- openFile "inputs/2.txt" ReadMode
    -- total <- processFile handle 1 0
    -- print total
    -- hClose handle

    handle <- openFile "inputs/2.txt" ReadMode
    total <- processFile2 handle 1 0
    print total
    hClose handle

processFile :: Handle -> Int -> Int -> IO Int
processFile handle lineNo sumSoFar = do
    eof <- hIsEOF handle
    if eof
        then return sumSoFar
        else do
            line <- hGetLine handle
            let newSum = if validLine line 
                then sumSoFar + lineNo
                else sumSoFar
            processFile handle (lineNo + 1) newSum

processFile2 :: Handle -> Int -> Int -> IO Int
processFile2 handle lineNo sumSoFar = do
    eof <- hIsEOF handle
    if eof
        then return sumSoFar
        else do
            line <- hGetLine handle
            let power = linePower line
            processFile2 handle (lineNo + 1) (sumSoFar + power)

-- max red < 12, max g < 13, max b < 14
validLine :: String -> Bool
validLine line = let
    tokens = splitOnDelimiters line
    redCounts = extractColorCount "red" tokens
    greenCounts = extractColorCount "green" tokens
    blueCounts = extractColorCount "blue" tokens

    maxRed = if null redCounts then 0 else maximum redCounts
    maxGreen = if null greenCounts then 0 else maximum greenCounts
    maxBlue = if null blueCounts then 0 else maximum blueCounts
    
    conditionMet = maxRed <= 12 && maxGreen <= 13 && maxBlue <= 14
    in trace ("-------------------------------------") $
       trace ("Line " ++ show line) $
       trace ("Tokens " ++ show tokens) $
       trace ("Red Counts " ++ show redCounts) $
       trace ("Red Max " ++ show maxRed) $
       trace ("-------------------------------------") $
       conditionMet

linePower :: String -> Int
linePower line = let
    tokens = splitOnDelimiters line
    redCounts = extractColorCount "red" tokens
    greenCounts = extractColorCount "green" tokens
    blueCounts = extractColorCount "blue" tokens

    maxRed = if null redCounts then 0 else maximum redCounts
    maxGreen = if null greenCounts then 0 else maximum greenCounts
    maxBlue = if null blueCounts then 0 else maximum blueCounts
    prod = maxRed * maxGreen * maxBlue 

    in prod 

isDelimiter :: Char -> Bool
isDelimiter c = c == ':' || c == ';' || c == ','

-- iterate through the sequence, breaking on any of the set delims, then filter out all the delims
splitOnDelimiters :: String -> [String]
splitOnDelimiters = filter (not . all isDelimiter) . groupBy (\a b -> not (isDelimiter a || isDelimiter b))

extractColorCount :: String -> [String] -> [Int]
extractColorCount color = let 
        colorBlocks = filter (color `isInfixOf`) 
        blockCounts = map (read . filter isDigit) . colorBlocks
        in blockCounts