{-# LANGUAGE OverloadedStrings
              #-}

module Kinesis.Layout where

import Control.Applicative

import Control.Monad

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.ByteString.Builder as BU

import Data.Foldable

import qualified Data.Map.Strict as M

import qualified Kinesis.Action as KA
import qualified Kinesis.Location as KL

data TapAndHold = TapAndHold Int KA.Action
                deriving (Show)

data Remap = Remap KL.Location KA.Action (Maybe TapAndHold)
           deriving (Show)

type RemapMap = M.Map KL.Location (KA.Action, Maybe TapAndHold)

toRemapMap :: [Remap] -> RemapMap
toRemapMap = foldl' (\m (Remap l a mth) -> M.insert l (a, mth) m) M.empty

fromRemapMap :: RemapMap -> [Remap]
fromRemapMap = map (\(l, (a, mth)) -> Remap l a mth) . M.toList

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
    a <- KA.parseAction ']'
    pure (TapAndHold d a)

parseRemap :: A.Parser Remap
parseRemap = do
    skipSpaceTab
    A.string "["
    l <- KL.parseLocation ']'
    skipSpaceTab
    A.string ">"
    skipSpaceTab
    A.string "["
    a <- KA.parseAction ']'
    skipSpaceTab
    mth <- optional parseTapAndHold
    skipSpaceTab
    A.endOfLine
    pure (Remap l a mth)

renderRemap :: Remap -> BU.Builder
renderRemap (Remap l a mth) =
    mconcat ( "["
            : BU.byteString (KL.locationTok l)
            : "]>["
            : BU.byteString (KA.actionTok a)
            :  "]"
            : (case mth of
                   Nothing -> ["\r\n"]
                   Just (TapAndHold d ha) ->
                       [ "[t&h"
                       , BU.intDec d
                       , "]["
                       , BU.byteString (KA.actionTok ha)
                       , "]\r\n"
                       ]))

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
    pure v
    where d125 = A.stringCI "d125}" *> pure Delay125
          d500 = A.stringCI "d500}" *> pure Delay500
          speed = do
              A.stringCI "speed"
              s <- A.decimal
              when ((s < 1) || (s > 9))
                  (fail "speed out of range")
              A.string "}"
              pure (Speed s)
          keyUpDown = do
              ud <- asum [ A.char '+' *> pure KeyUp
                         , A.char '-' *> pure KeyDown
                         ]
              a <- KA.parseAction '}'
              pure (ud a)
          key = Key <$> KA.parseAction '}'

renderMacroPart :: MacroPart -> BU.Builder
renderMacroPart (Key a) = BU.byteString (KA.actionTok a)
renderMacroPart (KeyDown a) = "-" <> BU.byteString (KA.actionTok a)
renderMacroPart (KeyUp a) = "+" <> BU.byteString (KA.actionTok a)
renderMacroPart (Speed s) = "speed" <> BU.intDec s
renderMacroPart Delay125 = "d125"
renderMacroPart Delay500 = "d500"

parseMacroParts :: A.Parser [MacroPart]
parseMacroParts =
    (   (parseMacroPart >>= (\p -> (p:) <$> parseMacroParts))
    <|> (skipSpaceTab *> A.endOfLine *> pure [])
    )

parseTriggerPart :: A.Parser KL.Location
parseTriggerPart = do
    skipSpaceTab
    A.string "{"
    l <- KL.parseLocation '}'
    skipSpaceTab
    pure l

parseTriggerParts :: A.Parser [KL.Location]
parseTriggerParts =
    (   (parseTriggerPart >>= (\p -> (p:) <$> parseTriggerParts))
    <|> (skipSpaceTab *> A.string ">" *> pure [])
    )

data Macro = Macro [KL.Location] [MacroPart]
           deriving (Show)

type MacroMap = M.Map [KL.Location] [MacroPart]

toMacroMap :: [Macro] -> MacroMap
toMacroMap = foldl' (\m (Macro l p) -> M.insert l p m) M.empty

fromMacroMap :: MacroMap -> [Macro]
fromMacroMap = map (\(l, p) -> Macro l p) . M.toList

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

renderMacro :: Macro -> BU.Builder
renderMacro (Macro ls ps) = mconcat [mconcat rls, ">", mconcat rps, "\r\n"]
    where rls = map (\l -> "{" <> BU.byteString (KL.locationTok l) <> "}") ls
          rps = map (\p -> "{" <> renderMacroPart p <> "}") ps

data Layout = Layout RemapMap MacroMap
            deriving (Show)

parseLayoutLine :: A.Parser (Either Remap Macro)
parseLayoutLine =
    asum [ parseComment *> parseLayoutLine
         , Left <$> parseRemap
         , Right <$> parseMacro
         ]

parseLayout :: A.Parser Layout
parseLayout = go id id
    where go rs ms = (A.endOfInput
                      *> pure (Layout (toRemapMap (rs []))
                                      (toMacroMap (ms []))))
                 <|> next rs ms
          next rs ms = do
              erm <- parseLayoutLine
              case erm of
                  Left r  -> go (rs . (r:)) ms
                  Right m -> go rs (ms . (m:))

renderLayout :: Layout -> BU.Builder
renderLayout (Layout rm mm) =
    mconcat [ mconcat (map renderRemap (fromRemapMap rm))
            , mconcat (map renderMacro (fromMacroMap mm))
            ]
