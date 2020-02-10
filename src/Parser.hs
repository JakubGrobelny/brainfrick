module Parser where

import           Instruction
import           Text.Megaparsec
import           Data.Void
import           Data.Text
import           Text.Megaparsec.Char

type Parser = Parsec Void Text


parseInstructions :: Parser [Instruction]
parseInstructions = many instruction
  where
    instruction :: Parser Instruction
    instruction = choice
        [ LShift <$ char '<'
        , RShift <$ char '>'
        , Increment <$ char '+'
        , Decrement <$ char '-'
        , Print <$ char '.'
        , Read <$ char ','
        , Loop <$> loopBody
        ]
    loopBody :: Parser [Instruction]
    loopBody = do
        char '['
        body <- many instruction
        char ']'
        return body
