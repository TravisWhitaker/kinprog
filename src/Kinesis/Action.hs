{-# LANGUAGE OverloadedStrings
           #-}

module Kinesis.Action (
    Action(..)
  , parseAction
  , actionTok
  ) where

import Control.Applicative

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC

import Data.Foldable

import Data.Word

import Prelude hiding (Left, Right)

data Action = F1
            | F2
            | F3
            | F4
            | F5
            | F6
            | F7
            | F8
            | F9
            | F10
            | F11
            | F12
            | F13
            | F14
            | F15
            | F16
            | F17
            | F18
            | F19
            | F20
            | F21
            | F22
            | F23
            | F24
            | One
            | Two
            | Three
            | Four
            | Five
            | Six
            | Seven
            | Eight
            | Nine
            | Zero
            | Grave
            | Hyphen
            | Equals
            | A
            | B
            | C
            | D
            | E
            | F
            | G
            | H
            | I
            | J
            | K
            | L
            | M
            | N
            | O
            | P
            | Q
            | R
            | S
            | T
            | U
            | V
            | W
            | X
            | Y
            | Z
            | Backslash
            | Colon
            | Quote
            | Comma
            | Period
            | Slash
            | OpenBrack
            | CloseBrack
            | LShift
            | LSuper
            | LCtrl
            | LAlt
            | RShift
            | RSuper
            | RCtrl
            | RAlt
            | Meta
            | Hyper
            | Next
            | Prev
            | Play
            | Mute
            | VolMinus
            | VolPlus
            | Enter
            | Tab
            | Space
            | Delete
            | Backspace
            | Insert
            | Home
            | PageUp
            | PageDown
            | Left
            | Right
            | Up
            | Down
            | End
            | Escape
            | PrtScr
            | ScrollLock
            | CapsLock
            | Pause
            | Calc
            | Shutdown
            | Intl
            | Menu
            | Null
            | KPToggle
            | KPShift
            | NPZero
            | NPOne
            | NPTwo
            | NPThree
            | NPFour
            | NPFive
            | NPSix
            | NPSeven
            | NPEight
            | NPNine
            | NumLock
            | NPPeriod
            | NPEquals
            | NPEqualsMac
            | NPDiv
            | NPPlus
            | NPMult
            | NPMinus
            | NPEnter
            | HIDCode Int
            deriving (Eq, Show)

tok :: BS.ByteString -> Action -> A.Parser Action
tok n a = A.stringCI n *> pure a

parseAction :: A.Parser Action
parseAction = toks <|> code
    where toks = asum
                  [ tok "f1" F1
                  , tok "f2" F2
                  , tok "f3" F3
                  , tok "f4" F4
                  , tok "f5" F5
                  , tok "f6" F6
                  , tok "f7" F7
                  , tok "f8" F8
                  , tok "f9" F9
                  , tok "f10" F10
                  , tok "f11" F11
                  , tok "f12" F12
                  , tok "f13" F13
                  , tok "f14" F14
                  , tok "f15" F15
                  , tok "f16" F16
                  , tok "f17" F17
                  , tok "f18" F18
                  , tok "f19" F19
                  , tok "f20" F20
                  , tok "f21" F21
                  , tok "f22" F22
                  , tok "f23" F23
                  , tok "f24" F24
                  , tok "1" One
                  , tok "2" Two
                  , tok "3" Three
                  , tok "4" Four
                  , tok "5" Five
                  , tok "6" Six
                  , tok "7" Seven
                  , tok "8" Eight
                  , tok "9" Nine
                  , tok "0" Zero
                  , tok "`" Grave
                  , tok "hyphen" Hyphen
                  , tok "=" Equals
                  , tok "a" A
                  , tok "b" B
                  , tok "c" C
                  , tok "d" D
                  , tok "e" E
                  , tok "f" F
                  , tok "g" G
                  , tok "h" H
                  , tok "i" I
                  , tok "j" J
                  , tok "k" K
                  , tok "l" L
                  , tok "m" M
                  , tok "n" N
                  , tok "o" O
                  , tok "p" P
                  , tok "q" Q
                  , tok "r" R
                  , tok "s" S
                  , tok "t" T
                  , tok "u" U
                  , tok "v" V
                  , tok "w" W
                  , tok "x" X
                  , tok "y" Y
                  , tok "z" Z
                  , tok "\\" Backslash
                  , tok ";" Colon
                  , tok "'" Quote
                  , tok "," Comma
                  , tok "." Period
                  , tok "/" Slash
                  , tok "obrack" OpenBrack
                  , tok "cbrack" CloseBrack
                  , tok "lshift" LShift
                  , tok "lwin" LSuper
                  , tok "lctrl" LCtrl
                  , tok "lalt" LAlt
                  , tok "rshift" RShift
                  , tok "rwin" RSuper
                  , tok "rctrl" RCtrl
                  , tok "ralt" RAlt
                  , tok "meh" Meta
                  , tok "hyper" Hyper
                  , tok "next" Next
                  , tok "prev" Prev
                  , tok "play" Play
                  , tok "mute" Mute
                  , tok "vol-" VolMinus
                  , tok "vol+" VolPlus
                  , tok "enter" Enter
                  , tok "tab" Tab
                  , tok "space" Space
                  , tok "delete" Delete
                  , tok "bspace" Backspace
                  , tok "insert" Insert
                  , tok "home" Home
                  , tok "pup" PageUp
                  , tok "pdown" PageDown
                  , tok "left" Left
                  , tok "right" Right
                  , tok "up" Up
                  , tok "down" Down
                  , tok "end" End
                  , tok "escape" Escape
                  , tok "prtscr" PrtScr
                  , tok "scroll" ScrollLock
                  , tok "caps" CapsLock
                  , tok "pause" Pause
                  , tok "calc" Calc
                  , tok "shutdn" Shutdown
                  , tok "intl-\\" Intl
                  , tok "menu" Menu
                  , tok "null" Null
                  , tok "kptoggle" KPToggle
                  , tok "kpshift" KPShift
                  , tok "kp0" NPZero
                  , tok "kp1" NPOne
                  , tok "kp2" NPTwo
                  , tok "kp3" NPThree
                  , tok "kp4" NPFour
                  , tok "kp5" NPFive
                  , tok "kp6" NPSix
                  , tok "kp7" NPSeven
                  , tok "kp8" NPEight
                  , tok "kp9" NPNine
                  , tok "numlk" NumLock
                  , tok "kp." NPPeriod
                  , tok "kp=" NPEquals
                  , tok "kp=mac" NPEqualsMac
                  , tok "kpdiv" NPDiv
                  , tok "kpplus" NPPlus
                  , tok "kpmult" NPMult
                  , tok "kpmin" NPMinus
                  , tok "kpenter1" NPEnter
                  ]
          code = do
              c <- A.decimal :: A.Parser Int
              if (c >= 0) && (c < 232)
              then pure (HIDCode c)
              else fail "HIDCode out of range"

actionTok :: Action -> BS.ByteString
actionTok F1 = "f1"
actionTok F2 = "f2"
actionTok F3 = "f3"
actionTok F4 = "f4"
actionTok F5 = "f5"
actionTok F6 = "f6"
actionTok F7 = "f7"
actionTok F8 = "f8"
actionTok F9 = "f9"
actionTok F10 = "f10"
actionTok F11 = "f11"
actionTok F12 = "f12"
actionTok F13 = "f13"
actionTok F14 = "f14"
actionTok F15 = "f15"
actionTok F16 = "f16"
actionTok F17 = "f17"
actionTok F18 = "f18"
actionTok F19 = "f19"
actionTok F20 = "f20"
actionTok F21 = "f21"
actionTok F22 = "f22"
actionTok F23 = "f23"
actionTok F24 = "f24"
actionTok One = "1"
actionTok Two = "2"
actionTok Three = "3"
actionTok Four = "4"
actionTok Five = "5"
actionTok Six = "6"
actionTok Seven = "7"
actionTok Eight = "8"
actionTok Nine = "9"
actionTok Zero = "0"
actionTok Grave = "`"
actionTok Hyphen = "hyphen"
actionTok Equals = "="
actionTok A = "a"
actionTok B = "b"
actionTok C = "c"
actionTok D = "d"
actionTok E = "e"
actionTok F = "f"
actionTok G = "g"
actionTok H = "h"
actionTok I = "i"
actionTok J = "j"
actionTok K = "k"
actionTok L = "l"
actionTok M = "m"
actionTok N = "n"
actionTok O = "o"
actionTok P = "p"
actionTok Q = "q"
actionTok R = "r"
actionTok S = "s"
actionTok T = "t"
actionTok U = "u"
actionTok V = "v"
actionTok W = "w"
actionTok X = "x"
actionTok Y = "y"
actionTok Z = "z"
actionTok Backslash = "\\"
actionTok Colon = ";"
actionTok Quote = "'"
actionTok Comma = ","
actionTok Period = "."
actionTok Slash = "/"
actionTok OpenBrack = "obrack"
actionTok CloseBrack = "cbrack"
actionTok LShift = "lshift"
actionTok LSuper = "lwin"
actionTok LCtrl = "lctrl"
actionTok LAlt = "lalt"
actionTok RShift = "rshift"
actionTok RSuper = "rwin"
actionTok RCtrl = "rctrl"
actionTok RAlt = "ralt"
actionTok Meta = "meg"
actionTok Hyper = "hyper"
actionTok Next = "next"
actionTok Prev = "pref"
actionTok Play = "play"
actionTok Mute = "mute"
actionTok VolMinus = "vol-"
actionTok VolPlus = "vol+"
actionTok Enter = "enter"
actionTok Tab = "tab"
actionTok Space = "space"
actionTok Delete = "delete"
actionTok Backspace = "bspace"
actionTok Insert = "insert"
actionTok Home = "home"
actionTok PageUp = "pup"
actionTok PageDown = "down"
actionTok Left = "left"
actionTok Right = "right"
actionTok Up = "up"
actionTok Down = "down"
actionTok End = "end"
actionTok Escape = "escape"
actionTok PrtScr = "prtscr"
actionTok ScrollLock = "scroll"
actionTok CapsLock = "caps"
actionTok Pause = "pause"
actionTok Calc = "calc"
actionTok Shutdown = "shutdn"
actionTok Intl = "intl-\\"
actionTok Menu = "menu"
actionTok Null = "null"
actionTok KPToggle = "kptoggle"
actionTok KPShift = "kpshift"
actionTok NPZero = "kp0"
actionTok NPOne = "kp1"
actionTok NPTwo = "kp2"
actionTok NPThree = "kp3"
actionTok NPFour = "kp4"
actionTok NPFive = "kp5"
actionTok NPSix = "kp6"
actionTok NPSeven = "kp7"
actionTok NPEight = "kp8"
actionTok NPNine = "kp9"
actionTok NumLock = "numlk"
actionTok NPPeriod = "kp."
actionTok NPEquals = "kp="
actionTok NPEqualsMac = "kp=mac"
actionTok NPDiv = "kpdiv"
actionTok NPPlus = "kpplus"
actionTok NPMult = "kpmult"
actionTok NPMinus = "kpmin"
actionTok NPEnter = "kpenter1"
-- kinda lame
actionTok (HIDCode w) = BC.pack (show w)
