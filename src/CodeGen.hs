module CodeGen where

import Optimizer


emitC :: String -> [OptimizedInstruction] -> IO ()
emitC file instructions = print instructions