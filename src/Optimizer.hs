{-# LANGUAGE DeriveFunctor #-}

module Optimizer
    ( optimizeInstructions
    , interactiveOptimization
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
    | AssignMultiply Offset Int
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

interactiveOptimization :: [I.Instruction] -> IO ()
interactiveOptimization xs = do
    let instructions = convertInstructions xs
    print instructions
    interactiveOptimization' instructions
  where
    interactiveOptimization' :: [OptimizedInstruction] -> IO ()
    interactiveOptimization' xs = do
        let optimized = optimizerPass $ InProgress xs
        if isDone optimized
            then return ()
            else do
                print . unwrap $ optimized
                interactiveOptimization' $ unwrap optimized

optimizerPass :: OptimizationResult -> OptimizationResult
optimizerPass xs = optimize $ unwrap xs
  where
    optimize :: [OptimizedInstruction] -> OptimizationResult
    optimize xs = case xs of
        Shift n : Shift m : xs -> InProgress $ Shift (n + m) : xs
        Loop [] : xs -> InProgress xs
        Loop body : xs -> case optimize body of
            Done body -> (Loop body :) <$> optimize xs
            InProgress body -> InProgress $ Loop body : xs
        Add n offset : Shift m : xs ->
            InProgress $ Shift m : Add n (offset - m) : xs
        Add n offset0 : Add m offset1 : xs | offset0 == offset1 ->
            InProgress $ Add (n + m) offset1 : xs
        Shift n : Add k offset : Shift m : xs | n * m < 0 ->
            InProgress $ Add k (offset + n) : Shift (n + m) : xs
        Shift 0 : xs -> InProgress xs
        y       : ys -> (y :) <$> optimize ys
        []           -> Done []
