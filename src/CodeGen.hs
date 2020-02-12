{-# LANGUAGE OverloadedStrings #-}

module CodeGen
    ( emitC
    )
where

import           Optimizer                      ( OptimizedInstruction(..) )
import           Control.Monad
import           Data.Text               hiding ( map )
import           Data.Text.IO
import           Prelude                 hiding ( appendFile
                                                , writeFile
                                                , concat
                                                )

tapeSize :: Text
tapeSize = toText $ 2 ^ 16

toText :: Show a => a -> Text
toText = pack . show

cBoilerplateTop :: Text
cBoilerplateTop =
    "#include <stdio.h>\n\n"
        <> "const unsigned long long tape_size = "
        <> tapeSize
        <> ";\n"
        <> "char tape["
        <> tapeSize
        <> "];\n\n"
        <> "int main() {\n"
        <> "    char* ptr = tape;\n"

cBoilerplateEnd :: Text
cBoilerplateEnd = "    return 0;\n}\n"

emitC :: String -> [OptimizedInstruction] -> IO ()
emitC file instructions = do
    writeFile file cBoilerplateTop
    forM_ instructions (appendFile file . indent . translateInstruction)
    appendFile file cBoilerplateEnd
  where
    indent :: Text -> Text
    indent = (<> "\n") . ("    " <>)
    translateInstruction :: OptimizedInstruction -> Text
    translateInstruction (Shift offset) = "ptr +=" <> toText offset <> ";"
    translateInstruction (Add val offset) =
        "*(ptr + " <> toText offset <> ") += " <> toText val <> ";"
    translateInstruction (MultiplyAdd mult offset) =
        "*(ptr + " <> toText offset <> ") += *ptr * " <> toText mult <> ";"
    translateInstruction (Clear offset) =
        "*(ptr + " <> toText offset <> ") = 0;"
    translateInstruction (Print offset) =
        "putchar(*(ptr + " <> toText offset <> "));"
    translateInstruction (Read offset) =
        "*(ptr + " <> toText offset <> ") = getchar();"
    translateInstruction (Block instructions) =
        concat $ map (indent . translateInstruction) instructions
    translateInstruction (Loop body) =
        "while (*ptr) {\n"
            <> concat (map (("    " <>) . indent . translateInstruction) body)
            <> "}\n"
