module Optimizer where

import qualified Instruction as I

type Offset = Int
type Value = Int

data OptimizedInstruction
    = Shift Offset
    | Add Value Offset
    | Clear Offset
    | Print Offset
    | Read Offset
    | Loop [OptimizedInstruction]
    deriving Show

convertInstructions :: [I.Instruction] -> [OptimizedInstruction]
convertInstructions = map convertInstruction

convertInstruction :: I.Instruction -> OptimizedInstruction
convertInstruction (I.RShift n) = Shift n
convertInstruction (I.LShift n) = Shift (-n)
convertInstruction I.Print      = Print 0
convertInstruction I.Read       = Read 0
convertInstruction (I.Increment n) = Add n 0
convertInstruction (I.Decrement n) = Add (-n) 0
convertInstruction (I.Loop xs) = Loop $ convertInstructions xs