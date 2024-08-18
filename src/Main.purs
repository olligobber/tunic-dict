module Main where

import Prelude (Unit, unit, pure, ($), (>>=), (<>), map)
import Data.Array (intercalate)
import Data.Set.NonEmpty (cons)
import Data.Set (fromFoldable)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Halogen.Svg.Attributes as SA

import Tunic.Glyph
import Tunic.Drawing (drawWord)
import Drawing (simpleScaleOffset, renderDrawing, rescale)

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI component unit

myWords :: Array Word
myWords =
  [ [ cons (Circ CircSegment) $ fromFoldable
      [ Vowel TopRight
      , Vowel TopLeft
      , Vowel Left
      , Consonant UpLeft
      , Consonant DownLeft
      , Consonant DownRight
      ]
    ]
  , [ cons (Circ CircSegment) $ fromFoldable
      [ Vowel TopLeft
      , Vowel TopRight
      , Vowel Left
      , Consonant Up
      , Consonant DownLeft
      , Consonant DownRight
      ]
    , cons (Vowel Left) $ fromFoldable
      [ Vowel BottomLeft
      , Vowel BottomRight
      , Consonant Up
      , Consonant UpLeft
      , Consonant DownRight
      ]
    , cons (Consonant UpLeft) $ fromFoldable
      [ Consonant DownLeft
      , Consonant DownRight
      ]
    , cons (Consonant Down) $ fromFoldable
      [ Consonant UpLeft
      , Vowel TopRight
      , Vowel Left
      , Vowel BottomLeft
      , Vowel BottomRight
      ]
    ]
  ]

component :: forall q m. H.Component q Unit Unit m
component = H.mkComponent
  { initialState : pure unit
  , render : pure $ HH.div [] $
    [HH.text "Let's go on "] <>
    intercalate [HH.text " "] (map
      (\w -> [renderDrawing (rescale 18.0 (drawWord w) simpleScaleOffset) [SA.class_ $ HH.ClassName "inlinesvg"]])
      myWords
      )
    <>
    [HH.text "!"]
  , eval : H.mkEval $ H.defaultEval
  }