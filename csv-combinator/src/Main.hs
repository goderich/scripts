{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

data Lemma = Lemma
    { semanticDomain :: !String
    , meaning :: !String
    , rest :: V.Vector String
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
