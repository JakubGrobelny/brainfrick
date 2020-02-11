{-# LANGUAGE DeriveFunctor #-}

module Optimizer
    ( optimizeInstructions
    , OptimizedInstruction
    )
where

import           Data.Either
import qualified Instruction                   as I

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
  where
    convertInstruction :: I.Instruction -> OptimizedInstruction
    convertInstruction (I.RShift n)     = Shift n
    convertInstruction (I.LShift n)     = Shift (-n)
    convertInstruction I.Print          = Print 0
    convertInstruction I.Read           = Read 0
    convertInstruction (I.Increment n ) = Add n 0
    convertInstruction (I.Decrement n ) = Add (-n) 0
    convertInstruction (I.Loop      xs) = Loop $ convertInstructions xs

data Result a
    = Done a
    | InProgress a
  deriving Functor

type OptimizationResult = Result [OptimizedInstruction]

unwrap :: Result a -> a
unwrap (Done       a) = a
unwrap (InProgress a) = a

isDone :: Result a -> Bool
isDone (Done _) = True
isDone _        = False

optimizeInstructions :: [I.Instruction] -> [OptimizedInstruction]
optimizeInstructions xs = unwrap $ until isDone optimizerPass xs'
    where xs' = InProgress $ convertInstructions xs

optimizerPass :: OptimizationResult -> OptimizationResult
optimizerPass xs = optimize $ unwrap xs
  where
    optimize :: [OptimizedInstruction] -> OptimizationResult
    optimize xs = case xs of
        Shift n : Shift m : xs -> InProgress $ Shift (n + m) : xs
        Add n offset0 : Add m offset1 : xs | offset0 == offset1 ->
            InProgress $ Add (n + m) offset0 : xs
        Shift n : Add k offset : Shift m : xs | n * m < 0 ->
            InProgress $ Add k n : Shift (n + m) : xs
        Shift 0 : xs -> InProgress xs
        y : ys -> (y :) <$> optimize ys
        []     -> Done []
















