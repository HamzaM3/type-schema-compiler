{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import Control.Applicative (Alternative (..), (<**>))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (find)
import Data.Tuple (swap)
import ParsingUtils (mapSnd, (:+:))

-------------------------------------

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ fmap (mapSnd f) . p

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ Just . (,x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser p) (Parser p') = Parser $ \input -> do
    (rest, ast) <- p input
    (rest', arg) <- p' rest
    return (rest', ast arg)

  (<*) :: Parser a -> Parser b -> Parser a
  (<*) (Parser p) (Parser p') = Parser $ \input ->
    do
      (rest, out) <- p input
      (rest', _) <- p' rest
      return (rest', out)

  (*>) :: Parser a -> Parser b -> Parser b
  (*>) (Parser p) (Parser p') = Parser $ \input ->
    do
      (rest, _) <- p input
      (rest', out) <- p' rest
      return (rest', out)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p) (Parser p') = Parser $ \s -> p s <|> p' s

parseSpace :: Parser String
parseSpace = Parser $ Just . swap . span (\x -> isSpace x && x /= '\n')

parseWhiteSpace :: Parser String
parseWhiteSpace = Parser $ Just . swap . span isSpace

(</*>) :: Parser (a -> b) -> Parser a -> Parser b
(</*>) p p' = p <* parseSpace <*> p'

(</*) :: Parser a -> Parser b -> Parser a
(</*) p p' = p <* parseSpace <* p'

(*\>) :: Parser a -> Parser b -> Parser b
(*\>) p p' = p <* parseSpace *> p'

infixl 4 </*>

infixl 4 </*

infixl 4 *\>

(<//*>) :: Parser (a -> b) -> Parser a -> Parser b
(<//*>) p p' = p <* parseWhiteSpace <*> p'

(<//*) :: Parser a -> Parser b -> Parser a
(<//*) p p' = p <* parseWhiteSpace <* p'

(*\\>) :: Parser a -> Parser b -> Parser b
(*\\>) p p' = p <* parseWhiteSpace *> p'

infixl 4 <//*>

infixl 4 <//*

infixl 4 *\\>

getData :: Parser a -> (String -> Maybe a)
getData (Parser p) = fmap snd . p

extract :: Parser a -> String -> Maybe a
extract p = fmap snd . runParser p

riskyExtract :: Parser a -> String -> a
riskyExtract p s = case extract p s of
  Just x -> x

parseFile :: Parser a -> Parser [a]
parseFile p = Parser $ \input -> do
  (rest, out) <- runParser p input
  (_, out') <- if rest /= "" then runParser (parseFile p) rest else Just ("", [])
  return ("", out : out')

---