{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

data Lemma = Lemma
    { semanticDomain :: !String
    , meaning :: !String
    , rest :: [String]
    } deriving Show

instance FromRecord Lemma where
    parseRecord r = Lemma <$>
                    r .! 0 <*>
                    r .! 1 <*>
                    parseRecord (V.tail . V.tail $ r)

main :: IO ()
main = do
    csvData <- BL.readFile "test/data.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> 
            V.forM_ v $ \p@Lemma {} -> print p
