{-# LANGUAGE OverloadedStrings
           #-}

module Kinesis.Location (
    Location(..)
  , parseLocation
  , locationTok
  ) where

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.ByteString as BS

import Data.Foldable

import Prelude hiding (Left, Right)

data Location = Escape
              | F1
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
              | PrtScr
              | ScrollLock
              | Pause
              | Equals
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
              | Hyphen
              | Tab
              | Q
              | W
              | E
              | R
              | T
              | Y
              | U
              | I
              | O
              | P
              | Backslash
              | CapsLock
              | A
              | S
              | D
              | F
              | G
              | H
              | J
              | K
              | L
              | Colon
              | Quote
              | LShift
              | Z
              | X
              | C
              | V
              | B
              | N
              | M
              | Comma
              | Period
              | Slash
              | RShift
              | Grave
              | Intl
              | Left
              | Right
              | Up
              | Down
              | OpenBrack
              | CloseBrack
              | LCtrl
              | LAlt
              | Backspace
              | Delete
              | Home
              | End
              | RSuper
              | RCtrl
              | PageUp
              | PageDown
              | Enter
              | Space
              | LPedal
              | MPedal
              | RPedal
              | KPEscape
              | KPLSuper
              | KPRAlt
              | Menu
              | Play
              | Prev
              | Next
              | Calc
              | KPShift
              | KPF9
              | KPF10
              | KPF11
              | KPF12
              | Mute
              | VolMinus
              | VolPlus
              | KPEquals
              | KPOne
              | KPTwo
              | KPThree
              | KPFour
              | KPFive
              | KPSix
              | NumLock
              | KPNPEquals
              | KPNPDiv
              | KPNPMult
              | KPHyphen
              | KPTab
              | KPQ
              | KPW
              | KPE
              | KPR
              | KPT
              | KPY
              | KPNPSeven
              | KPNPEight
              | KPNPNine
              | KPNPMinus
              | KPBackslash
              | KPCapsLock
              | KPA
              | KPS
              | KPD
              | KPF
              | KPG
              | KPH
              | KPNPFour
              | KPNPFive
              | KPNPSix
              | KPPlus
              | KPQuote
              | KPLShift
              | KPZ
              | KPX
              | KPC
              | KPV
              | KPB
              | KPN
              | KPNPOne
              | KPNPTwo
              | KPNPThree
              | KPEnter1
              | KPRShift
              | KPGrave
              | KPInsert
              | KPLeft
              | KPRight
              | KPUp
              | KPDown
              | KPPeriod
              | KPEnter2
              | KPLCtrl
              | KPLAlt
              | KPBackspace
              | KPDelete
              | KPHome
              | KPEnd
              | KPRSuper
              | KPRCtrl
              | KPPageUp
              | KPPageDown
              | KPEnter
              | KPNPZero
              | KPLPedal
              | KPMPedal
              | KPRPedal
              deriving (Eq, Ord, Show, Enum, Bounded)

tok :: Char -> BS.ByteString -> Location -> A.Parser Location
tok t n a = A.stringCI n *> A.char t *> pure a

parseLocation :: Char -> A.Parser Location
parseLocation t = asum
    [ tok t "escape" Escape
    , tok t "F1" F1
    , tok t "F2" F2
    , tok t "F3" F3
    , tok t "F4" F4
    , tok t "F5" F5
    , tok t "F6" F6
    , tok t "F7" F7
    , tok t "F8" F8
    , tok t "F9" F9
    , tok t "F10" F10
    , tok t "F11" F11
    , tok t "F12" F12
    , tok t "prtscr" PrtScr
    , tok t "scroll" ScrollLock
    , tok t "pause" Pause
    , tok t "=" Equals
    , tok t "1" One
    , tok t "2" Two
    , tok t "3" Three
    , tok t "4" Four
    , tok t "5" Five
    , tok t "6" Six
    , tok t "7" Seven
    , tok t "8" Eight
    , tok t "9" Nine
    , tok t "0" Zero
    , tok t "hyphen" Hyphen
    , tok t "tab" Tab
    , tok t "q" Q
    , tok t "w" W
    , tok t "e" E
    , tok t "r" R
    , tok t "t" T
    , tok t "y" Y
    , tok t "u" U
    , tok t "i" I
    , tok t "o" O
    , tok t "p" P
    , tok t "\\" Backslash
    , tok t "caps" CapsLock
    , tok t "a" A
    , tok t "s" S
    , tok t "d" D
    , tok t "f" F
    , tok t "g" G
    , tok t "h" H
    , tok t "j" J
    , tok t "k" K
    , tok t "l" L
    , tok t ";" Colon
    , tok t "'" Quote
    , tok t "lshift" LShift
    , tok t "z" Z
    , tok t "x" X
    , tok t "c" C
    , tok t "v" V
    , tok t "b" B
    , tok t "n" N
    , tok t "m" M
    , tok t "," Comma
    , tok t "." Period
    , tok t "/" Slash
    , tok t "rshift" RShift
    , tok t "`" Grave
    , tok t "intl-\\" Intl
    , tok t "left" Left
    , tok t "right" Right
    , tok t "up" Up
    , tok t "down" Down
    , tok t "obrack" OpenBrack
    , tok t "cbrack" CloseBrack
    , tok t "lctrl" LCtrl
    , tok t "lalt" LAlt
    , tok t "bspace" Backspace
    , tok t "delete" Delete
    , tok t "home" Home
    , tok t "end" End
    , tok t "rwin" RSuper
    , tok t "rctrl" RCtrl
    , tok t "pup" PageUp
    , tok t "pdown" PageDown
    , tok t "enter" Enter
    , tok t "space" Space
    , tok t "lp-tab" LPedal
    , tok t "mp-kpshf" MPedal
    , tok t "rp-kpent" RPedal
    , tok t "kp-escape" KPEscape
    , tok t "kp-lwin" KPLSuper
    , tok t "kp-ralt" KPRAlt
    , tok t "menu" Menu
    , tok t "play" Play
    , tok t "prev" Prev
    , tok t "next" Next
    , tok t "calc" Calc
    , tok t "kpshft" KPShift
    , tok t "kp-f9" KPF9
    , tok t "kp-f10" KPF10
    , tok t "kp-f11" KPF11
    , tok t "kp-f12" KPF12
    , tok t "mute" Mute
    , tok t "vol-" VolMinus
    , tok t "vol+" VolPlus
    , tok t "kp-=" KPEquals
    , tok t "kp-1" KPOne
    , tok t "kp-2" KPTwo
    , tok t "kp-3" KPThree
    , tok t "kp-4" KPFour
    , tok t "kp-5" KPFive
    , tok t "kp-6" KPSix
    , tok t "numlk" NumLock
    , tok t "kp=" KPNPEquals
    , tok t "kpdiv" KPNPDiv
    , tok t "kpmult" KPNPMult
    , tok t "kp-hyphen" KPHyphen
    , tok t "kp-tab" KPTab
    , tok t "kp-q" KPQ
    , tok t "kp-w" KPW
    , tok t "kp-e" KPE
    , tok t "kp-r" KPR
    , tok t "kp-t" KPT
    , tok t "kp-y" KPY
    , tok t "kp7" KPNPSeven
    , tok t "kp8" KPNPEight
    , tok t "kp9" KPNPNine
    , tok t "kpmin" KPNPMinus
    , tok t "kp-\\" KPBackslash
    , tok t "kp-caps" KPCapsLock
    , tok t "kp-a" KPA
    , tok t "kp-s" KPS
    , tok t "kp-d" KPD
    , tok t "kp-f" KPF
    , tok t "kp-g" KPG
    , tok t "kp-h" KPH
    , tok t "kp4" KPNPFour
    , tok t "kp5" KPNPFive
    , tok t "kp6" KPNPSix
    , tok t "kp7" KPPlus
    , tok t "kp-'" KPQuote
    , tok t "kp-lshift" KPLShift
    , tok t "kp-z" KPZ
    , tok t "kp-x" KPX
    , tok t "kp-c" KPC
    , tok t "kp-v" KPV
    , tok t "kp-b" KPB
    , tok t "kp-n" KPN
    , tok t "kp1" KPNPOne
    , tok t "kp2" KPNPTwo
    , tok t "kp3" KPNPThree
    , tok t "kpenter1" KPEnter1
    , tok t "kp-rshift" KPRShift
    , tok t "kp-`" KPGrave
    , tok t "kp-insert" KPInsert
    , tok t "kp-left" KPLeft
    , tok t "kp-right" KPRight
    , tok t "kp-up" KPUp
    , tok t "kp-down" KPDown
    , tok t "kp." KPPeriod
    , tok t "kpenter2" KPEnter2
    , tok t "kp-lctrl" KPLCtrl
    , tok t "kp-lalt" KPLAlt
    , tok t "kp-bspace" KPBackspace
    , tok t "kp-delete" KPDelete
    , tok t "kp-home" KPHome
    , tok t "kp-end" KPEnd
    , tok t "kp-rwin" KPRSuper
    , tok t "kp-rctrl" KPRCtrl
    , tok t "kp-pup" KPPageUp
    , tok t "kp-pdown" KPPageDown
    , tok t "kp-enter" KPEnter
    , tok t "kp0" KPNPZero
    , tok t "kp-lp-tab" KPLPedal
    , tok t "kp-mp-kpshf" KPMPedal
    , tok t "kp-rp-kpent" KPRPedal
    ]

locationTok :: Location -> BS.ByteString
locationTok Escape = "escape"
locationTok F1 = "f1"
locationTok F2 = "f2"
locationTok F3 = "f3"
locationTok F4 = "f4"
locationTok F5 = "f5"
locationTok F6 = "f6"
locationTok F7 = "f7"
locationTok F8 = "f8"
locationTok F9 = "f9"
locationTok F10 = "f10"
locationTok F11 = "f11"
locationTok F12 = "f12"
locationTok PrtScr = "prtscr"
locationTok ScrollLock = "scroll"
locationTok Pause = "pause"
locationTok Equals = "="
locationTok One = "1"
locationTok Two = "2"
locationTok Three = "3"
locationTok Four = "4"
locationTok Five = "5"
locationTok Six = "6"
locationTok Seven = "7"
locationTok Eight = "8"
locationTok Nine = "9"
locationTok Zero = "0"
locationTok Hyphen = "hyphen"
locationTok Tab = "tab"
locationTok Q = "q"
locationTok W = "w"
locationTok E = "e"
locationTok R = "r"
locationTok T = "t"
locationTok Y = "y"
locationTok U = "u"
locationTok I = "i"
locationTok O = "o"
locationTok P = "p"
locationTok Backslash = "\\"
locationTok CapsLock = "caps"
locationTok A = "a"
locationTok S = "s"
locationTok D = "d"
locationTok F = "f"
locationTok G = "g"
locationTok H = "h"
locationTok J = "j"
locationTok K = "k"
locationTok L = "l"
locationTok Colon = ";"
locationTok Quote = "'"
locationTok LShift = "lshift"
locationTok Z = "z"
locationTok X = "x"
locationTok C = "c"
locationTok V = "v"
locationTok B = "b"
locationTok N = "n"
locationTok M = "m"
locationTok Comma = ","
locationTok Period = "."
locationTok Slash = "/"
locationTok RShift = "rshift"
locationTok Grave = "`"
locationTok Intl = "intl-\\"
locationTok Left = "left"
locationTok Right = "right"
locationTok Up = "up"
locationTok Down = "down"
locationTok OpenBrack = "obrack"
locationTok CloseBrack = "cbrack"
locationTok LCtrl = "lctrl"
locationTok LAlt = "lalt"
locationTok Backspace = "bspace"
locationTok Delete = "delete"
locationTok Home = "home"
locationTok End = "end"
locationTok RSuper = "rwin"
locationTok RCtrl = "rctrl"
locationTok PageUp = "pup"
locationTok PageDown = "pdown"
locationTok Enter = "enter"
locationTok Space = "space"
locationTok LPedal = "lp-tab"
locationTok MPedal = "mp-kpshf"
locationTok RPedal = "rp-kpent"
locationTok KPEscape = "kp-escape"
locationTok KPLSuper = "kp-lwin"
locationTok KPRAlt = "kp-ralt"
locationTok Menu = "menu"
locationTok Play = "play"
locationTok Prev = "prev"
locationTok Next = "next"
locationTok Calc = "calc"
locationTok KPShift = "kpshift"
locationTok KPF9 = "kp-f9"
locationTok KPF10 = "kp-f10"
locationTok KPF11 = "kp-f11"
locationTok KPF12 = "kp-f12"
locationTok Mute = "mute"
locationTok VolMinus = "vol-"
locationTok VolPlus = "vol+"
locationTok KPEquals = "kp-="
locationTok KPOne = "kp-1"
locationTok KPTwo = "kp-2"
locationTok KPThree = "kp-3"
locationTok KPFour = "kp-4"
locationTok KPFive = "kp-5"
locationTok KPSix = "kp-6"
locationTok NumLock = "numlk"
locationTok KPNPEquals = "kp="
locationTok KPNPDiv = "kpdiv"
locationTok KPNPMult = "kpmult"
locationTok KPHyphen = "kp-hyphen"
locationTok KPTab = "kp-tab"
locationTok KPQ = "kp-q"
locationTok KPW = "kp-w"
locationTok KPE = "kp-e"
locationTok KPR = "kp-r"
locationTok KPT = "kp-t"
locationTok KPY = "kp-y"
locationTok KPNPSeven = "kp7"
locationTok KPNPEight = "kp8"
locationTok KPNPNine = "kp9"
locationTok KPNPMinus = "kpmin"
locationTok KPBackslash = "kp-\\"
locationTok KPCapsLock = "kp-caps"
locationTok KPA = "kp-a"
locationTok KPS = "kp-s"
locationTok KPD = "kp-d"
locationTok KPF = "kp-f"
locationTok KPG = "kp-g"
locationTok KPH = "kp-h"
locationTok KPNPFour = "kp4"
locationTok KPNPFive = "kp5"
locationTok KPNPSix = "kp6"
locationTok KPPlus = "kpplus"
locationTok KPQuote = "kp-'"
locationTok KPLShift = "kp-lshift"
locationTok KPZ = "kp-z"
locationTok KPX = "kp-x"
locationTok KPC = "kp-c"
locationTok KPV = "kp-v"
locationTok KPB = "kp-b"
locationTok KPN = "kp-n"
locationTok KPNPOne = "kp1"
locationTok KPNPTwo = "kp2"
locationTok KPNPThree = "kp3"
locationTok KPEnter1 = "kpenter1"
locationTok KPRShift = "kp-rshift"
locationTok KPGrave = "kp-`"
locationTok KPInsert = "kp-insert"
locationTok KPLeft = "kp-left"
locationTok KPRight = "kp-right"
locationTok KPUp = "kp-up"
locationTok KPDown = "kp-down"
locationTok KPPeriod = "kp."
locationTok KPEnter2 = "kpenter2"
locationTok KPLCtrl = "kp-lctrl"
locationTok KPLAlt = "kp-lalt"
locationTok KPBackspace = "kp-bspace"
locationTok KPDelete = "kp-delete"
locationTok KPHome = "kp-home"
locationTok KPEnd = "kp-end"
locationTok KPRSuper = "kp-rwin"
locationTok KPRCtrl = "kp-rctrl"
locationTok KPPageUp = "kp-pup"
locationTok KPPageDown = "kp-pdown"
locationTok KPEnter = "kp-enter"
locationTok KPNPZero = "kp0"
locationTok KPLPedal = "kp-lp-tab"
locationTok KPMPedal = "kp-mp-kpshf"
locationTok KPRPedal = "kp-rp-kpent"
