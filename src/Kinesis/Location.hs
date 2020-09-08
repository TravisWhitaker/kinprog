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

tok :: BS.ByteString -> Location -> A.Parser Location
tok n a = A.stringCI n *> pure a

parseLocation :: A.Parser Location
parseLocation = asum
    [ tok "escape" Escape
    , tok "F1" F1
    , tok "F2" F2
    , tok "F3" F3
    , tok "F4" F4
    , tok "F5" F5
    , tok "F6" F6
    , tok "F7" F7
    , tok "F8" F8
    , tok "F9" F9
    , tok "F10" F10
    , tok "F11" F11
    , tok "F12" F12
    , tok "prtscr" PrtScr
    , tok "scroll" ScrollLock
    , tok "pause" Pause
    , tok "=" Equals
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
    , tok "hyphen" Hyphen
    , tok "tab" Tab
    , tok "q" Q
    , tok "w" W
    , tok "e" E
    , tok "r" R
    , tok "t" T
    , tok "y" Y
    , tok "u" U
    , tok "i" I
    , tok "o" O
    , tok "p" P
    , tok "\\" Backslash
    , tok "caps" CapsLock
    , tok "a" A
    , tok "s" S
    , tok "d" D
    , tok "f" F
    , tok "g" G
    , tok "h" H
    , tok "j" J
    , tok "k" K
    , tok "l" L
    , tok ";" Colon
    , tok "'" Quote
    , tok "lshift" LShift
    , tok "z" Z
    , tok "x" X
    , tok "c" C
    , tok "v" V
    , tok "b" B
    , tok "n" N
    , tok "m" M
    , tok "," Comma
    , tok "." Period
    , tok "/" Slash
    , tok "rshift" RShift
    , tok "`" Grave
    , tok "intl-\\" Intl
    , tok "left" Left
    , tok "right" Right
    , tok "up" Up
    , tok "down" Down
    , tok "obrack" OpenBrack
    , tok "cbrack" CloseBrack
    , tok "lctrl" LCtrl
    , tok "lalt" LAlt
    , tok "bspace" Backspace
    , tok "delete" Delete
    , tok "home" Home
    , tok "end" End
    , tok "rwin" RSuper
    , tok "rctrl" RCtrl
    , tok "pup" PageUp
    , tok "pdown" PageDown
    , tok "enter" Enter
    , tok "space" Space
    , tok "lp-tab" LPedal
    , tok "mp-kpshf" MPedal
    , tok "rp-kpent" RPedal
    , tok "kp-escape" KPEscape
    , tok "kp-lwin" KPLSuper
    , tok "kp-ralt" KPRAlt
    , tok "menu" Menu
    , tok "play" Play
    , tok "prev" Prev
    , tok "next" Next
    , tok "calc" Calc
    , tok "kpshft" KPShift
    , tok "kp-f9" KPF9
    , tok "kp-f10" KPF10
    , tok "kp-f11" KPF11
    , tok "kp-f12" KPF12
    , tok "mute" Mute
    , tok "vol-" VolMinus
    , tok "vol+" VolPlus
    , tok "kp-=" KPEquals
    , tok "kp-1" KPOne
    , tok "kp-2" KPTwo
    , tok "kp-3" KPThree
    , tok "kp-4" KPFour
    , tok "kp-5" KPFive
    , tok "kp-6" KPSix
    , tok "numlk" NumLock
    , tok "kp=" KPNPEquals
    , tok "kpdiv" KPNPDiv
    , tok "kpmult" KPNPMult
    , tok "kp-hyphen" KPHyphen
    , tok "kp-tab" KPTab
    , tok "kp-q" KPQ
    , tok "kp-w" KPW
    , tok "kp-e" KPE
    , tok "kp-r" KPR
    , tok "kp-t" KPT
    , tok "kp-y" KPY
    , tok "kp7" KPNPSeven
    , tok "kp8" KPNPEight
    , tok "kp9" KPNPNine
    , tok "kpmin" KPNPMinus
    , tok "kp-\\" KPBackslash
    , tok "kp-caps" KPCapsLock
    , tok "kp-a" KPA
    , tok "kp-s" KPS
    , tok "kp-d" KPD
    , tok "kp-f" KPF
    , tok "kp-g" KPG
    , tok "kp-h" KPH
    , tok "kp4" KPNPFour
    , tok "kp5" KPNPFive
    , tok "kp6" KPNPSix
    , tok "kp7" KPPlus
    , tok "kp-'" KPQuote
    , tok "kp-lshift" KPLShift
    , tok "kp-z" KPZ
    , tok "kp-x" KPX
    , tok "kp-c" KPC
    , tok "kp-v" KPV
    , tok "kp-b" KPB
    , tok "kp-n" KPN
    , tok "kp1" KPNPOne
    , tok "kp2" KPNPTwo
    , tok "kp3" KPNPThree
    , tok "kpenter1" KPEnter1
    , tok "kp-rshift" KPRShift
    , tok "kp-`" KPGrave
    , tok "kp-insert" KPInsert
    , tok "kp-left" KPLeft
    , tok "kp-right" KPRight
    , tok "kp-up" KPUp
    , tok "kp-down" KPDown
    , tok "kp." KPPeriod
    , tok "kpenter2" KPEnter2
    , tok "kp-lctrl" KPLCtrl
    , tok "kp-lalt" KPLAlt
    , tok "kp-bspace" KPBackspace
    , tok "kp-delete" KPDelete
    , tok "kp-home" KPHome
    , tok "kp-end" KPEnd
    , tok "kp-rwin" KPRSuper
    , tok "kp-rctrl" KPRCtrl
    , tok "kp-pup" KPPageUp
    , tok "kp-pdown" KPPageDown
    , tok "kp-enter" KPEnter
    , tok "kp0" KPNPZero
    , tok "kp-lp-tab" KPLPedal
    , tok "kp-mp-kpshf" KPMPedal
    , tok "kp-rp-kpent" KPRPedal
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
