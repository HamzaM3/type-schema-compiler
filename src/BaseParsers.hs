{-# LANGUAGE TupleSections #-}

module BaseParsers where

import Control.Applicative (Alternative (..), (<**>))
import Data.Char (isAlpha, isDigit)
import Data.Tuple (swap)
import Parser (Parser (..))
import ParsingUtils (mapSnd, nothingIf)

parseRest :: Parser String
parseRest = p <* (parseChar '\n' <|> pure 'a') -- turn maybe parsers into unfailable parser
  where
    p = Parser $ Just . swap . span ('\n' /=)

parseId :: Parser String
parseId = Parser $ Just . (,"")

parseInt :: Parser Int
parseInt = Parser $ fmap (mapSnd read) . nothingIf ((==) "" . snd) . swap . span isDigit

parseChar :: Char -> Parser Char
parseChar c = Parser p
  where
    p (u : rest) = if u == c then Just (rest, u) else Nothing
    p _ = Nothing

parseEnd :: Parser String
parseEnd = Parser $ \input -> nothingIf ((/=) "" . snd) (input, input)

parseAlpha :: Parser String
parseAlpha = Parser $ nothingIf ((==) "" . snd) . swap . span isAlpha

parseFail :: Parser a
parseFail = Parser $ const Nothing

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ nothingIf ((==) "" . snd) . swap . span f

parseSpanChar :: Char -> Parser String
parseSpanChar c = parseSpan (c ==)