{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( bar
    ) where

import Turtle
import qualified Data.Text as T
import qualified Control.Foldl as F

bar :: MonadIO io => io ()
bar = stdout $ fold b F.mconcat
  where b = (return "Battery: ") <|> formatBattText getBattery

getBattery :: Shell Line
getBattery = inproc "acpi" ["-b"] empty

formatBattText :: Shell Line -> Shell Line
formatBattText input = do
  result <- formatted input
  return $ maybe (fromString "ERROR") id result
  where formatted = fmap (textToLine
                         . flip T.snoc ' '
                         . T.replace "," ""
                         . flip (!!) 3
                         . T.words
                         . lineToText)
