module Main where

import           Parser
import           Optimizer
import           Text.Megaparsec
import           Data.Text
import           System.IO


readInput :: IO String
readInput = do
    eof <- isEOF
    if eof
        then return ""
        else do
            char <- getChar
            (char :) <$> readInput

main :: IO ()
main = do
    input <- pack <$> readInput
    case parse parseInstructions "" input of
        Left err -> putStr $ errorBundlePretty err
        Right instructions ->
            interactiveOptimization instructions    