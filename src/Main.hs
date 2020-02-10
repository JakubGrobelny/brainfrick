module Main where

import           Parser
import           Optimizer
import           Text.Megaparsec
import           Data.Text


main :: IO ()
main = interact $ show . parseAndOptimize . pack
  where
    parseAndOptimize :: Text -> Maybe [OptimizedInstruction]
    parseAndOptimize str = do
        instructions <- parseMaybe parseInstructions str
        return $ convertInstructions instructions