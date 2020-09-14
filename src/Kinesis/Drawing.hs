module Kinesis.Drawing where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Control.Monad

import qualified Data.Attoparsec.ByteString as A

import qualified Data.ByteString as BS

import qualified Graphics.Vty as V

import Kinesis.Layout

data KinState = KinState {
    ksLayout :: Layout
  } deriving (Show)

type Event = ()

type Name = ()

kinDraw :: KinState -> [Widget Name]
kinDraw (KinState l) = [board]
    where board = vBox [ chicRow
                       , r1
                       , r2
                       , r3r4
                       , r5
                       , thumbs
                       ]
          chicRow = padRight Max lChic <+> padLeft Max rChic
          r1 = padRight Max lr1 <+> padLeft Max rr1
          r2 = padRight Max lr2 <+> padLeft Max rr2
          r3r4 = (lr3 <=> lr4)
                  <+> hCenter pedals
                  <+> (rr3 <=> rr4)
          r5 = padRight Max lr5 <+> padLeft Max rr5
          thumbs = padRight Max (padLeft (Pad (7*5)) lThumb)
               <+> padLeft Max (padRight (Pad (7*5)) rThumb)
          lChic = hBox [ border (str "Esc")
                       , border (str "F1 ")
                       , border (str "F2 ")
                       , border (str "F3 ")
                       , border (str "F4 ")
                       , border (str "F5 ")
                       , border (str "F6 ")
                       , border (str "F7 ")
                       , border (str "F8 ")
                       ]
          rChic = hBox [ border (str "F9 ")
                       , border (str "F10")
                       , border (str "F11")
                       , border (str "F12")
                       , border (str "PSc")
                       , border (str "Scl")
                       , border (str "PB ")
                       , border (str "Kpd")
                       , border (str "Prg")
                       ]
          lr1 = hBox [ border (str "  =  ")
                     , border (str "  1  ")
                     , border (str "  2  ")
                     , border (str "  3  ")
                     , border (str "  4  ")
                     , border (str "  5  ")
                     ]
          rr1 = hBox [ border (str "  6  ")
                     , border (str "  7  ")
                     , border (str "  8  ")
                     , border (str "  9  ")
                     , border (str "  0  ")
                     , border (str "  -  ")
                     ]
          lr2 = hBox [ border (str " Tab ")
                     , border (str "  Q  ")
                     , border (str "  W  ")
                     , border (str "  E  ")
                     , border (str "  R  ")
                     , border (str "  T  ")
                     ]
          rr2 = hBox [ border (str "  Y  ")
                     , border (str "  U  ")
                     , border (str "  I  ")
                     , border (str "  O  ")
                     , border (str "  P  ")
                     , border (str "  \\  ")
                     ]
          lr3 = hBox [ border (str " Cap ")
                     , border (str "  A  ")
                     , border (str "  S  ")
                     , border (str "  D  ")
                     , border (str "  F  ")
                     , border (str "  G  ")
                     ]
          rr3 = hBox [ border (str "  H  ")
                     , border (str "  J  ")
                     , border (str "  K  ")
                     , border (str "  L  ")
                     , border (str "  ;  ")
                     , border (str "  '  ")
                     ]
          lr4 = hBox [ border (str " Shf ")
                     , border (str "  Z  ")
                     , border (str "  X  ")
                     , border (str "  C  ")
                     , border (str "  V  ")
                     , border (str "  B  ")
                     ]
          rr4 = hBox [ border (str "  N  ")
                     , border (str "  M  ")
                     , border (str "  ,  ")
                     , border (str "  .  ")
                     , border (str "  /  ")
                     , border (str " Shf ")
                     ]
          lr5 = padRight (Pad 7) $ padLeft (Pad 7) $ hBox
                  [ border (str "  `  ")
                  , border (str " Int ")
                  , border (str "  ←  ")
                  , border (str "  →  ")
                  ]
          rr5 = padRight (Pad 7) $ padLeft (Pad 7) $ hBox
                  [ border (str "  ↑  ")
                  , border (str "  ↓  ")
                  , border (str "  [  ")
                  , border (str "  ]  ")
                  ]
          padTall = padTop (Pad 1) . padBottom (Pad 2)
          lThumb = vBox
                    [ padLeft (Pad 7) (hBox [ border (str " Ctr ")
                                            , border (str " Alt ")
                                            ])
                    , hBox [ border (padTall (str " Bsp "))
                           , border (padTall (str " Del "))
                           , border (str " Hom ") <=> border (str " End ")
                           ]
                    ]
          rThumb = vBox
                    [ padRight (Pad 7) (hBox [ border (str " Win ")
                                             , border (str " Ctr ")
                                             ])
                    , hBox [ border (str " PUp ") <=> border (str " PDn ")
                           , border (padTall (str " Ret "))
                           , border (padTall (str " Spc "))
                           ]
                    ]
          pedals = hBox
                    [ border (padTall (str " LPd "))
                    , border (padTall (str " MPd "))
                    , border (padTall (str " Rpd "))
                    ]

kinEvent :: KinState -> BrickEvent Name Event -> EventM Name (Next KinState)
kinEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
kinEvent s _ = continue s

kinAttr :: AttrMap
kinAttr = attrMap V.defAttr []

kinApp :: App KinState Event Name
kinApp = App {
    appDraw = kinDraw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = kinEvent
  , appStartEvent = pure
  , appAttrMap = const kinAttr
  }

doApp :: IO ()
doApp = do
    q <- BS.readFile "/Users/travis/Desktop/qwerty.txt"
    let Right l = A.parseOnly parseLayout q
        initState = KinState l
    void $ defaultMain kinApp initState
