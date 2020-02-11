{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Console.GetOpt
import           System.Environment
import           Parser
import           Optimizer
import           Text.Megaparsec
import           Data.Text               hiding ( concat
                                                , foldl
                                                )
import qualified Data.Text.IO                  as TextIO
import           Data.Maybe                     ( fromMaybe )
import           System.IO
import           CodeGen

data Flag
    = Output String
    | Target String
  deriving Show

data Target
    = TargetC
    | TargetInvalid
  deriving(Show, Eq)

data Args = Args
    { argsInput :: String
    , argsOutput :: String
    , argsTarget :: Target
    }

parseArgs :: ([Flag], String) -> Args
parseArgs (flags, fileName) = foldl
    parse'
    (Args fileName (fileName ++ ".out") TargetC)
    flags
  where
    parse' :: Args -> Flag -> Args
    parse' args (Output out) = args { argsOutput = out }
    parse' args (Target "c") = args { argsTarget = TargetC }
    parse' args (Target _  ) = args { argsTarget = TargetInvalid }

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"] (ReqArg Output "FILE")   "output FILE"
    , Option ['t'] ["target"] (ReqArg Target "TARGET") "compilation target"
    ]

readFlags :: [String] -> IO ([Flag], String)
readFlags argv = case getOpt Permute options argv of
    (options, [fileName], []) -> return (options, fileName)
    (_, _, errors) ->
        ioError . userError $ concat errors ++ usageInfo header options
    where header = "Usage: brainfrick [OPTION...] file"

main :: IO ()
main = do
    argv <- getArgs
    args <- parseArgs <$> readFlags argv
    file         <- TextIO.readFile $ argsInput args
    instructions <- case parse parseInstructions "" file of
        Left  error        -> ioError . userError . errorBundlePretty $ error
        Right instructions -> return $ optimizeInstructions instructions
    case argsTarget args of
        TargetInvalid -> ioError . userError $ "Invalid target specified"
        TargetC -> emitC (argsOutput args) instructions