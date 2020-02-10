module Instruction where

data Instruction
    = LShift
    | RShift
    | Increment
    | Decrement
    | Print
    | Read
    | Loop [Instruction]
    deriving Show
