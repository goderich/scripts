{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

type Key = (String, String)
type Glosses = [String]

data Lemma = Lemma
    { semanticDomain :: String
    , meaning :: String
    , rest :: Glosses
    }

instance Show Lemma where
    show (Lemma d m r) = unlines [d, "meaning: " ++ m, show r]

instance FromRecord Lemma where
    parseRecord r = Lemma <$>
                    r .! 0 <*>
                    r .! 1 <*>
                    parseRecord (V.tail . V.tail $ r)

instance ToRecord Lemma where
    toRecord (Lemma d m gs) =
        record (toField d : toField m : map toField gs)

fromLemmata :: V.Vector Lemma -> M.Map Key [Glosses]
fromLemmata = M.fromListWith (flip (++)) . V.toList . V.map toKey 
    where toKey (Lemma d m r) = ((d,m), [r])

toLemmata :: M.Map Key [Glosses] -> V.Vector Lemma
toLemmata = V.fromList . concatMap unwrapLemmata . M.toList

unwrapLemmata :: (Key, [Glosses]) -> [Lemma]
unwrapLemmata ((d,m), rs) = map (Lemma d m) rs

combineLs :: [[String]] -> [[String]] -> [[String]]
combineLs xs ys = combineLsPadded xs ys l
    where l = length (head xs) + length (head ys)

-- Combines two lists of lists of Strings into a single list
-- such that the length of all embedded lists is the same.
-- If the length of outer lists is different, the inner lists
-- are padded with empty Strings.
combineLsPadded :: [[String]] -> [[String]] -> Int -> [[String]]
combineLsPadded [] [] _ = []
combineLsPadded (x:xs) [] l =
    (x ++ pad x l) : combineLsPadded xs [] l
combineLsPadded [] (y:ys) l =
    (pad y l ++ y) : combineLsPadded [] ys l
combineLsPadded (x:xs) (y:ys) l =
    (x ++ y) : combineLsPadded xs ys l

pad :: [a] -> Int -> [String]
pad x n = replicate (n - length x) ""

transformCSV :: Either String (V.Vector Lemma)
             -> Either String (V.Vector Lemma)
             -> Either String (V.Vector Lemma)
transformCSV xs ys = do
    v1 <- xs
    v2 <- ys
    let m1 = fromLemmata v1
    let m2 = fromLemmata v2
    let m3 = M.unionWith combineLs m1 m2
    pure $ toLemmata m3

main :: IO ()
main = do
    data1 <- BL.readFile "test/data.csv"
    data2 <- BL.readFile "test/data2.csv"
    let csv1 = decode HasHeader data1
    let csv2 = decode HasHeader data2
    let result = transformCSV csv1 csv2
    case result of
        Left err -> putStrLn err
        Right r  -> BL.writeFile "test/output.csv"
                  . encode . V.toList $ r
