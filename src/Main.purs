module Main where

import Prelude (Unit, pure, ($), (>>=), (<>), (==), mempty)
-- import Data.Array (intercalate)
-- import Data.Maybe (Maybe(..))
import Data.Set.NonEmpty (cons)
import Data.Set (Set, fromFoldable)
import Data.Set as S
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes (Color(Named))
-- import Web.HTML.Common (PropName(PropName))

import Tunic.Glyph
  ( CircSegment(..), ConsonantSegment(..), Segment(..), VowelSegment(..)
  , Word, fullGlyph
  )
import Tunic.Drawing (drawGlyph, simpleDraw, simpleDrawNoLine)
import Drawing (renderDrawing, group, DrawingProps(..))
import Tunic.Clickable (ClickEvent(..), clickArea)

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI component S.empty

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

-- component :: forall q m. H.Component q Unit Unit m
-- component = H.mkComponent
--   { initialState : pure unit
--   , render : pure $ HH.div [] $
--     [HH.text "Let's go on "] <>
--     intercalate [HH.text " "] (map
--       (\w -> [renderDrawing (drawWord w) [SA.class_ $ HH.ClassName "inlinesvg"]])
--       myWords
--       )
--     <>
--     [HH.text "!"]
--   , eval : H.mkEval $ H.defaultEval
--   }

type State = { selected :: Set Segment, hovered :: Set Segment }
initialState :: State
initialState = { selected : S.empty, hovered : S.empty }

component :: forall q m. H.Component q (Set Segment) ClickEvent m
component = H.mkComponent
  { initialState : pure initialState
  , render : \state -> HH.div_ [renderDrawing
    (group (DrawingProps [])
      (drawGlyph (simpleDrawNoLine $ Named "silver") fullGlyph mempty <>
      drawGlyph (simpleDraw $ Named "black") state.selected mempty <>
      drawGlyph (simpleDrawNoLine $ Named "red") state.hovered mempty
      ) <>
    group (DrawingProps []) clickArea)
    [SA.height 200.0]
    ]
  , eval : H.mkEval $ H.defaultEval { handleAction = \m -> case m of
      Hover s -> H.modify_ $ _ { hovered = s }
      Unhover s -> H.modify_ $ \t ->
        if t.hovered == s then
          t { hovered = S.empty }
        else
          t
      Click s -> H.modify_ $ \t -> t { selected =
        (S.difference t.selected s) <> (S.difference s t.selected)
      }
    }
  }