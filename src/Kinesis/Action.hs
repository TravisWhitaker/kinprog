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

import qualified Kinesis.Location as KL

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

-- | When programming remaps or macros on the keyboard, location tokens are
--   often generated where actions would be expected. For example, mapping
--   keypad layer Q to keypad layer A may be recorded by the keyboard as
--   @[kp-q]>[kp-a]@, where @[kp-q]>[a]@ would suffice. Although this is
--   inconsistent with the manual and redundant, each location has an
--   unambiguous action. This is the total function from locations to actions.
locationAction :: KL.Location -> Action
locationAction KL.Escape = Escape
locationAction KL.F1 = F1
locationAction KL.F2 = F2
locationAction KL.F3 = F3
locationAction KL.F4 = F4
locationAction KL.F5 = F5
locationAction KL.F6 = F6
locationAction KL.F7 = F7
locationAction KL.F8 = F8
locationAction KL.F9 = F9
locationAction KL.F10 = F10
locationAction KL.F11 = F11
locationAction KL.F12 = F12
locationAction KL.PrtScr = PrtScr
locationAction KL.ScrollLock = ScrollLock
locationAction KL.Pause = Pause
locationAction KL.Equals = Equals
locationAction KL.One = One
locationAction KL.Two = Two
locationAction KL.Three = Three
locationAction KL.Four = Four
locationAction KL.Five = Five
locationAction KL.Six = Six
locationAction KL.Seven = Seven
locationAction KL.Eight = Eight
locationAction KL.Nine = Nine
locationAction KL.Zero = Zero
locationAction KL.Hyphen = Hyphen
locationAction KL.Tab = Tab
locationAction KL.Q = Q
locationAction KL.W = W
locationAction KL.E = E
locationAction KL.R = R
locationAction KL.T = T
locationAction KL.Y = Y
locationAction KL.U = U
locationAction KL.I = I
locationAction KL.O = O
locationAction KL.P = P
locationAction KL.Backslash = Backslash
locationAction KL.CapsLock = CapsLock
locationAction KL.A = A
locationAction KL.S = S
locationAction KL.D = D
locationAction KL.F = F
locationAction KL.G = G
locationAction KL.H = H
locationAction KL.J = J
locationAction KL.K = K
locationAction KL.L = L
locationAction KL.Colon = Colon
locationAction KL.Quote = Quote
locationAction KL.LShift = LShift
locationAction KL.Z = Z
locationAction KL.X = X
locationAction KL.C = C
locationAction KL.V = V
locationAction KL.B = B
locationAction KL.N = N
locationAction KL.M = M
locationAction KL.Comma = Comma
locationAction KL.Period = Period
locationAction KL.Slash = Slash
locationAction KL.RShift = RShift
locationAction KL.Grave = Grave
locationAction KL.Intl = Intl
locationAction KL.Left = Left
locationAction KL.Right = Right
locationAction KL.Up = Up
locationAction KL.Down = Down
locationAction KL.OpenBrack = OpenBrack
locationAction KL.CloseBrack = CloseBrack
locationAction KL.LCtrl = LCtrl
locationAction KL.LAlt = LAlt
locationAction KL.Backspace = Backspace
locationAction KL.Delete = Delete
locationAction KL.Home = Home
locationAction KL.End = End
locationAction KL.RSuper = RSuper
locationAction KL.RCtrl = RCtrl
locationAction KL.PageUp = PageUp
locationAction KL.PageDown = PageDown
locationAction KL.Enter = Enter
locationAction KL.Space = Space
locationAction KL.LPedal = Tab
locationAction KL.MPedal = KPShift
locationAction KL.RPedal = NPEnter
locationAction KL.KPEscape = Escape
locationAction KL.KPLSuper = LSuper
locationAction KL.KPRAlt = RAlt
locationAction KL.Menu = Menu
locationAction KL.Play = Play
locationAction KL.Prev = Prev
locationAction KL.Next = Next
locationAction KL.Calc = Calc
locationAction KL.KPShift = KPShift
locationAction KL.KPF9 = F9
locationAction KL.KPF10 = F10
locationAction KL.KPF11 = F11
locationAction KL.KPF12 = F12
locationAction KL.Mute = Mute
locationAction KL.VolMinus = VolMinus
locationAction KL.VolPlus = VolPlus
locationAction KL.KPEquals = NPEquals
locationAction KL.KPOne = NPOne
locationAction KL.KPTwo = NPTwo
locationAction KL.KPThree = NPThree
locationAction KL.KPFour = NPFour
locationAction KL.KPFive = NPFive
locationAction KL.KPSix = NPSix
locationAction KL.NumLock = NumLock
locationAction KL.KPNPEquals = NPEquals
locationAction KL.KPNPDiv = NPDiv
locationAction KL.KPNPMult = NPMult
locationAction KL.KPHyphen = Hyphen
locationAction KL.KPTab = Tab
locationAction KL.KPQ = Q
locationAction KL.KPW = W
locationAction KL.KPE = E
locationAction KL.KPR = R
locationAction KL.KPT = T
locationAction KL.KPY = Y
locationAction KL.KPNPSeven = NPSeven
locationAction KL.KPNPEight = NPEight
locationAction KL.KPNPNine = NPNine
locationAction KL.KPNPMinus = NPMinus
locationAction KL.KPBackslash = Backslash
locationAction KL.KPCapsLock = CapsLock
locationAction KL.KPA = A
locationAction KL.KPS = S
locationAction KL.KPD = D
locationAction KL.KPF = F
locationAction KL.KPG = G
locationAction KL.KPH = H
locationAction KL.KPNPFour = NPFour
locationAction KL.KPNPFive = NPFive
locationAction KL.KPNPSix = NPSix
locationAction KL.KPPlus = NPPlus
locationAction KL.KPQuote = Quote
locationAction KL.KPLShift = LShift
locationAction KL.KPZ = Z
locationAction KL.KPX = X
locationAction KL.KPC = C
locationAction KL.KPV = V
locationAction KL.KPB = B
locationAction KL.KPN = N
locationAction KL.KPNPOne = NPOne
locationAction KL.KPNPTwo = NPTwo
locationAction KL.KPNPThree = NPThree
locationAction KL.KPEnter1 = NPEnter
locationAction KL.KPRShift = RShift
locationAction KL.KPGrave = Grave
locationAction KL.KPInsert = Insert
locationAction KL.KPLeft = Left
locationAction KL.KPRight = Right
locationAction KL.KPUp = Up
locationAction KL.KPDown = Down
locationAction KL.KPPeriod = NPPeriod
locationAction KL.KPEnter2 = NPEnter
locationAction KL.KPLCtrl = LCtrl
locationAction KL.KPLAlt = LAlt
locationAction KL.KPBackspace = Backspace
locationAction KL.KPDelete = Delete
locationAction KL.KPHome = Home
locationAction KL.KPEnd = End
locationAction KL.KPRSuper = RSuper
locationAction KL.KPRCtrl = RCtrl
locationAction KL.KPPageUp = PageUp
locationAction KL.KPPageDown = PageDown
locationAction KL.KPEnter = Enter
locationAction KL.KPNPZero = NPZero
locationAction KL.KPLPedal = Tab
locationAction KL.KPMPedal = KPShift
locationAction KL.KPRPedal = NPEnter

tok :: Char -> BS.ByteString -> Action -> A.Parser Action
tok t n a = A.stringCI n *> A.char t *> pure a

parseAction :: Char -> A.Parser Action
parseAction t = toks <|> code <|> loc
    where toks = asum
                  [ tok t "f1" F1
                  , tok t "f2" F2
                  , tok t "f3" F3
                  , tok t "f4" F4
                  , tok t "f5" F5
                  , tok t "f6" F6
                  , tok t "f7" F7
                  , tok t "f8" F8
                  , tok t "f9" F9
                  , tok t "f10" F10
                  , tok t "f11" F11
                  , tok t "f12" F12
                  , tok t "f13" F13
                  , tok t "f14" F14
                  , tok t "f15" F15
                  , tok t "f16" F16
                  , tok t "f17" F17
                  , tok t "f18" F18
                  , tok t "f19" F19
                  , tok t "f20" F20
                  , tok t "f21" F21
                  , tok t "f22" F22
                  , tok t "f23" F23
                  , tok t "f24" F24
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
                  , tok t "`" Grave
                  , tok t "hyphen" Hyphen
                  , tok t "=" Equals
                  , tok t "a" A
                  , tok t "b" B
                  , tok t "c" C
                  , tok t "d" D
                  , tok t "e" E
                  , tok t "f" F
                  , tok t "g" G
                  , tok t "h" H
                  , tok t "i" I
                  , tok t "j" J
                  , tok t "k" K
                  , tok t "l" L
                  , tok t "m" M
                  , tok t "n" N
                  , tok t "o" O
                  , tok t "p" P
                  , tok t "q" Q
                  , tok t "r" R
                  , tok t "s" S
                  , tok t "t" T
                  , tok t "u" U
                  , tok t "v" V
                  , tok t "w" W
                  , tok t "x" X
                  , tok t "y" Y
                  , tok t "z" Z
                  , tok t "\\" Backslash
                  , tok t ";" Colon
                  , tok t "'" Quote
                  , tok t "," Comma
                  , tok t "." Period
                  , tok t "/" Slash
                  , tok t "obrack" OpenBrack
                  , tok t "cbrack" CloseBrack
                  , tok t "lshift" LShift
                  , tok t "lwin" LSuper
                  , tok t "lctrl" LCtrl
                  , tok t "lalt" LAlt
                  , tok t "rshift" RShift
                  , tok t "rwin" RSuper
                  , tok t "rctrl" RCtrl
                  , tok t "ralt" RAlt
                  , tok t "meh" Meta
                  , tok t "hyper" Hyper
                  , tok t "next" Next
                  , tok t "prev" Prev
                  , tok t "play" Play
                  , tok t "mute" Mute
                  , tok t "vol-" VolMinus
                  , tok t "vol+" VolPlus
                  , tok t "enter" Enter
                  , tok t "tab" Tab
                  , tok t "space" Space
                  , tok t "delete" Delete
                  , tok t "bspace" Backspace
                  , tok t "insert" Insert
                  , tok t "home" Home
                  , tok t "pup" PageUp
                  , tok t "pdown" PageDown
                  , tok t "left" Left
                  , tok t "right" Right
                  , tok t "up" Up
                  , tok t "down" Down
                  , tok t "end" End
                  , tok t "escape" Escape
                  , tok t "prtscr" PrtScr
                  , tok t "scroll" ScrollLock
                  , tok t "caps" CapsLock
                  , tok t "pause" Pause
                  , tok t "calc" Calc
                  , tok t "shutdn" Shutdown
                  , tok t "intl-\\" Intl
                  , tok t "menu" Menu
                  , tok t "null" Null
                  , tok t "kptoggle" KPToggle
                  , tok t "kpshft" KPShift
                  , tok t "kp0" NPZero
                  , tok t "kp1" NPOne
                  , tok t "kp2" NPTwo
                  , tok t "kp3" NPThree
                  , tok t "kp4" NPFour
                  , tok t "kp5" NPFive
                  , tok t "kp6" NPSix
                  , tok t "kp7" NPSeven
                  , tok t "kp8" NPEight
                  , tok t "kp9" NPNine
                  , tok t "numlk" NumLock
                  , tok t "kp." NPPeriod
                  , tok t "kp=" NPEquals
                  , tok t "kp=mac" NPEqualsMac
                  , tok t "kpdiv" NPDiv
                  , tok t "kpplus" NPPlus
                  , tok t "kpmult" NPMult
                  , tok t "kpmin" NPMinus
                  , tok t "kpenter1" NPEnter
                  ]
          code = do
              c <- A.decimal :: A.Parser Int
              A.char t
              if (c >= 0) && (c < 232)
              then pure (HIDCode c)
              else fail "HIDCode out of range"
          loc = locationAction <$> KL.parseLocation t

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
actionTok KPShift = "kpshft"
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
