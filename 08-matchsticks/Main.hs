-- stack runghc --verbosity error

import Data.Char
import Text.Parsec
import Text.Parsec.String

escaped :: Parser Char
escaped = try (char '\\') *> (char '\\' <|> char '"' <|> hex)
  where hex = char 'x' *> (chr . read <$> count 2 hexDigit)

oneChar :: Parser Char
oneChar = escaped <|> anyChar

manyChars :: Parser String
-- manyChars = between (char '"') (char '"') $ many oneChar
manyChars = char '"' *> manyTill oneChar (char '"')

escape :: Char -> String
escape '\\' = "\\\\"
escape '"' = "\\\""
escape x = [x]

main :: IO ()
main = getContents >>= print . sum . map (uncurry (-) . counts) . lines
  -- where counts l = (length l, either (error . show) length $ parse manyChars l l)
  where counts l = (2 + length (concatMap escape l), length l)

