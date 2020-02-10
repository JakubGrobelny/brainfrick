module Parser where

import           Instruction
import           Text.Megaparsec
import           Data.Void
import           Data.Text               hiding ( length )
import           Text.Megaparsec.Char

type Parser = Parsec Void Text


parseInstructions :: Parser [Instruction]
parseInstructions = many instruction <* eof
  where
    comment :: Parser ()
    comment = skipMany $ noneOf "><,.+-[]"
    instruction :: Parser Instruction
    instruction =
        comment
            *> choice
                   [ LShift . length <$> some (char '<')
                   , RShift . length <$> some (char '>')
                   , Increment . length <$> some (char '+')
                   , Decrement . length <$> some (char '-')
                   , Print <$ char '.'
                   , Read <$ char ','
                   , Loop <$> loopBody
                   ]
            <* comment
    loopBody :: Parser [Instruction]
    loopBody = do
        char '['
        comment
        body <- many instruction
        comment
        char ']'
        return body