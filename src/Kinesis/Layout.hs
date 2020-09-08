{-# LANGUAGE OverloadedStrings
           #-}

module Kinesis.Layout where

import Control.Applicative

import Control.Monad

import qualified Data.Attoparsec.ByteString.Char8 as A

import Data.Foldable

import qualified Kinesis.Action as KA
import qualified Kinesis.Location as KL

data TapAndHold = TapAndHold Int KA.Action
                deriving (Show)

data Remap = Remap KL.Location KA.Action (Maybe TapAndHold)
           deriving (Show)

skipSpaceTab :: A.Parser ()
skipSpaceTab = A.skipWhile (\c -> (c == ' ') && (c == '\t'))

parseComment :: A.Parser ()
parseComment = do
    skipSpaceTab
    A.string "*"
    A.skipWhile (\c -> (c /= '\n') && (c /= '\r'))
    A.endOfLine

parseTapAndHold :: A.Parser TapAndHold
parseTapAndHold = do
    A.stringCI "[t&h"
    d <- A.decimal
    when ((d <= 0) || (d > 999))
        (fail "TapAndHold delay out of range")
    A.string "]"
    skipSpaceTab
    A.string "["
    a <- KA.parseAction
    A.string "]"
    pure (TapAndHold d a)

parseRemap :: A.Parser Remap
parseRemap = do
    skipSpaceTab
    A.string "["
    l <- KL.parseLocation
    A.string "]"
    skipSpaceTab
    A.string ">"
    skipSpaceTab
    A.string "["
    a <- KA.parseAction
    A.string "]"
    skipSpaceTab
    mth <- optional parseTapAndHold
    skipSpaceTab
    A.endOfLine
    pure (Remap l a mth)

data MacroPart = Key KA.Action
               | KeyDown KA.Action
               | KeyUp KA.Action
               | Speed Int
               | Delay125
               | Delay500
               deriving (Show)

parseMacroPart :: A.Parser MacroPart
parseMacroPart = do
    skipSpaceTab
    A.string "{"
    v <- asum [d125, d500, speed, keyUpDown, key]
    A.string "}"
    pure v
    where d125 = A.stringCI "d125" *> pure Delay125
          d500 = A.stringCI "d500" *> pure Delay500
          speed = do
              A.stringCI "speed"
              s <- A.decimal
              when ((s < 1) || (s > 9))
                  (fail "speed out of range")
              pure (Speed s)
          keyUpDown = do
              ud <- asum [ A.char '+' *> pure KeyUp
                         , A.char '-' *> pure KeyDown
                         ]
              a <- KA.parseAction
              pure (ud a)
          key = Key <$> KA.parseAction

parseMacroParts :: A.Parser [MacroPart]
parseMacroParts =
    (   (parseMacroPart >>= (\p -> (p:) <$> parseMacroParts))
    <|> (skipSpaceTab *> A.endOfLine *> pure [])
    )

parseTriggerPart :: A.Parser KL.Location
parseTriggerPart = do
    skipSpaceTab
    A.string "{"
    l <- KL.parseLocation
    A.string "}"
    skipSpaceTab
    pure l

parseTriggerParts :: A.Parser [KL.Location]
parseTriggerParts =
    (   (parseTriggerPart >>= (\p -> (p:) <$> parseTriggerParts))
    <|> (skipSpaceTab *> A.string ">" *> pure [])
    )

data Macro = Macro [KL.Location] [MacroPart]
           deriving (Show)

parseMacro :: A.Parser Macro
parseMacro = do
    ts <- parseTriggerParts
    when (null ts)
        (fail "no macro trigger")
    -- parseTriggerParts consumed the > for us
    ms <- parseMacroParts
    when (null ts)
        (fail "no macro actions")
    -- parseMacroParts consumed the line end for us
    pure (Macro ts ms)

data Layout = Layout [Remap] [Macro]
            deriving (Show)

parseLayoutLine :: A.Parser (Either Remap Macro)
parseLayoutLine =
    asum [ parseComment *> parseLayoutLine
         , Left <$> parseRemap
         , Right <$> parseMacro
         ]

parseLayout :: A.Parser Layout
parseLayout = go id id
    where go rs ms = (A.endOfInput *> pure (Layout (rs []) (ms [])))
                 <|> next rs ms
          next rs ms = do
              erm <- parseLayoutLine
              case erm of
                  Left r  -> go (rs . (r:)) ms
                  Right m -> go rs (ms . (m:))
