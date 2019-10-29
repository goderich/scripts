{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import System.Environment (getArgs)
import Control.Monad (zipWithM_)

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
fromLemmata = M.fromListWith (flip (++)) . V.toList . fmap toKey 
    where toKey (Lemma d m r) = ((d,m), [r])

toLemmata :: M.Map Key [Glosses] -> V.Vector Lemma
toLemmata = V.fromList . concatMap unwrapLemmata . M.toList

unwrapLemmata :: (Key, [Glosses]) -> [Lemma]
unwrapLemmata ((d,m), rs) = map (Lemma d m) rs

combineMatrices :: Monoid a => [[a]] -> [[a]] -> [[a]]
combineMatrices xs ys
  | null xs = ys
  | length xs > length ys = zipWith (++) xs (pad (length xs - length ys) ys)
  | length xs < length ys = zipWith (++) (pad (length ys - length xs) xs) ys
  | otherwise = zipWith (++) xs ys

pad :: Monoid a => Int -> [[a]] -> [[a]]
pad len xs = xs ++ (replicate len
                          (replicate (length . head $ xs) mempty))

transformCSV :: Either String (V.Vector Lemma)
             -> Either String (V.Vector Lemma)
             -> Either String (V.Vector Lemma)
transformCSV xs ys = do
    v1 <- xs
    v2 <- ys
    -- l = total amount of gloss columns in two tables
    let l = sum . map (length . rest . V.head) $ [v1, v2]
    let m1 = fromLemmata v1
    let m2 = fromLemmata v2
    let m3 = M.merge
               -- Preserve keys only found in m1
               M.preserveMissing
               -- Drop keys only found in m2
               M.dropMissing
               (M.zipWithMatched (\_ x y -> combineMatrices x y))
               m1 m2
    pure . fmap (padLemma l) . toLemmata $ m3

-- Makes sure every row has the same number of columns.
-- This is only needed when m2 has missing keys.
padLemma :: Int -> Lemma -> Lemma
padLemma l (Lemma d m gs) = Lemma d m (gs ++ pad gs l)
  where pad x n = replicate (n - length x) ""

diffCSVs :: Either String (V.Vector Lemma)
         -> Either String (V.Vector Lemma)
         -> Either String (V.Vector Lemma)
diffCSVs xs ys = do
    v1 <- xs
    v2 <- ys
    let m1 = fromLemmata v1
    let m2 = fromLemmata v2
    let diff = M.difference m1 m2
    pure . toLemmata $ diff

printCSV :: Either String (V.Vector Lemma) -> String -> IO ()
printCSV x name = case x of
    Left err -> putStrLn err
    Right r  -> BL.writeFile name . encode . V.toList $ r

main :: IO ()
main = do
    args <- getArgs
    files <- mapM BL.readFile args
    origF <- BL.readFile "test/original.csv"
    let original = decode HasHeader origF
    let csvs = map (decode HasHeader) files
    let diffs = map (diffCSVs original) csvs
    zipWithM_ printCSV diffs (map (++ "-diff") args)
    let merged = foldl transformCSV original csvs
    printCSV merged "test/output.csv"
