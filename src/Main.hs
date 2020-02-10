module Main where

import           Parser
import           Text.Megaparsec
import           Data.Text


main :: IO ()
main = interact $ show . parseMaybe parseInstructions . pack
