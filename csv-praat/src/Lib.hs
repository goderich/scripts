{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( readCSV )
where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (intercalate)

-- A program that takes a CSV file in the following format as input:
--
-- (begin,end,text)
-- #2:30.7#,#2:32.8#,"kyan mistasway suQ(也有可能)_你有沒有兄弟姐妹Q"
--
-- ...and produces the following Praat TextGrid output:
--
-- intervals [4]:
--     xmin = 150.7
--     xmax = 152.8
--     text = "kyan mistasway suQ(也有可能)_你有沒有兄弟姐妹Q"

data CSVLine = CSVLine { begin :: MinSec, end :: MinSec, text :: String }
             deriving (Eq, Show)

-- Needed because the Parlatype timestamp format is MM:SS.T, whereas
-- Praat uses a Float for the number of seconds.
data MinSec = MinSec { mins :: Int, secs :: Int, tenths :: Int }
            deriving (Eq, Show)

type Parser = Parsec Void String

readCSV :: IO ()
readCSV = do
  inputFile <- readFile input
  let lines = parseMaybe (many parseCSVLine) inputFile
  case lines of
    Nothing -> fail "parse error"
    Just parsed ->
      writeFile output
      . concat
      . ((header ++ show len ++ "\n") :)
      . zipWith appendIntervalNum [1..]
      . map transform
      $ intervals
      where intervals = padTimeIntervals parsed
            len       = length intervals

-- Using `intercalate` instead of `unlines` because I'm appending the number of
-- intervals in the main function, so I don't want a trailing newline here.
header = intercalate "\n"
  [ "File type = \"ooTextFile\""
  , "Object class = \"TextGrid\""
  , ""
  , "xmin = 0"
  , "xmax = " ++ show (toSecs xmax)
  , "tiers? <exists>"
  , "size = 1"
  , "item []:"
  , "    item [1]:"
  , "        class = \"IntervalTier\""
  , "        name = \"Transcription\""
  , "        xmin = 0"
  , "        xmax = " ++ show (toSecs xmax) ++ ""
  , "        intervals: size = "]

-- xmin is pretty much always gonna be 0, but xmax has to be hardcoded for now
xmin = MinSec 0 0 0
xmax = MinSec m s 5
  where (m, s) = divMod 3587 60

transform :: CSVLine -> String
transform (CSVLine b e t) = unlines . map ((replicate 12 ' ') ++ ) $
  [ "xmin = " ++ show (toSecs b)
  , "xmax = " ++ show (toSecs e)
  , "text = \"" ++ t ++ "\""]

toSecs :: MinSec -> Float
toSecs (MinSec m s t) = fromIntegral ((m * 60) + s) + (fromIntegral t / 10)

appendIntervalNum :: Int -> String -> String
appendIntervalNum x s = (replicate 8 ' ') ++ "intervals [" ++ show x ++ "]:\n" ++ s

-- This function assumes that any manually sliced intervals don't coincide with either
-- the beginning or the end of the file.
padTimeIntervals :: [CSVLine] -> [CSVLine]
padTimeIntervals xs = [h] ++ (padInternal xs) ++ [t]
  where h = CSVLine xmin (begin $ head xs) ""
        t = CSVLine (end $ last xs) xmax ""

padInternal :: [CSVLine] -> [CSVLine]
padInternal (x:xs)
  | xs == [] = [x]
  | (end x) == (begin y) = x : y : padInternal (tail xs)
  | otherwise = x : (CSVLine (end x) (begin y) "") : padInternal (xs)
  where y = head xs

parseCSVLine :: Parser CSVLine
parseCSVLine = do
  b <- parseMinSec
  e <- parseMinSec
  t <- parsePhrase
  return $ CSVLine b e t

parseMinSec :: Parser MinSec
parseMinSec = do
  char '#'
  m <- some digitChar
  char ':'
  s <- count 2 digitChar
  char '.'
  t <- some digitChar
  char '#' >> char ','
  return $ MinSec (read m) (read s) (read t)

parsePhrase :: Parser String
parsePhrase = do
  phrase <- count 1 (parseSentence <|> parseWord)
  optional newline
  return $ head phrase

parseSentence :: Parser String
parseSentence = do
  char '"'
  sentence <- some (phraseChar <|> spaceChar)
  char '"'
  return sentence

parseWord :: Parser String
parseWord = some phraseChar

phraseChar :: Parser Char
phraseChar = alphaNumChar <|> char '_' <|> char '(' <|> char ')'
