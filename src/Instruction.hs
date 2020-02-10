module Instruction where

data Instruction
    = LShift Int
    | RShift Int
    | Increment Int
    | Decrement Int
    | Print
    | Read
    | Loop [Instruction]
    deriving Show