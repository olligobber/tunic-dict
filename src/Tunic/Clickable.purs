module Tunic.Clickable
  ( ClickEvent(..)
  , clickArea
) where

import Prelude
  ( ($), (||), (&&), (==), (<>), (<$>), compare, min, max, mempty, negate, pure
  , class Semigroup, class Monoid, class Eq, class Ord
  )
import Control.Alt ((<|>))
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as M
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes.Color (Color(RGBA, NoColor))
import Web.HTML.Common (PropName(..))

import Drawing
  ( Drawing, Horizontal(..), Vertical(..), Point2D(..), DrawingProps(..)
  , line, circle, withoutViewport
  )
import Tunic.Glyph (Segment, fullGlyph)
import Tunic.Drawing (DrawFunc, drawGlyph)

transparent :: Color
transparent = RGBA 0 0 0 0.0

data Line = Line Point2D Point2D

instance eqLine :: Eq Line where
  eq (Line a b) (Line c d) =
    ((a == c) && (b == d)) ||
    ((a == d) && (b == c))

instance ordLine :: Ord Line where
  compare (Line a b) (Line c d) =
    compare [min a b, max a b] [min c d, max c d]

newtype ClickData = ClickData
  { lines :: Map Line (Set Segment)
  , circles :: Maybe { center :: Point2D, radius :: Number, segment :: Segment }
  , corners :: Map Point2D (Set Segment)
  }

instance semigroupClickData :: Semigroup ClickData where
  append (ClickData a) (ClickData b) = ClickData
    { lines : M.unionWith (<>) a.lines b.lines
    , circles : a.circles <|> b.circles
    , corners : M.unionWith (<>) a.corners b.corners
    }

instance monoidClickData :: Monoid ClickData where
  mempty = ClickData { lines : M.empty, circles : Nothing, corners : M.empty }

drawClickData :: DrawFunc ClickData
drawClickData =
  { drawLine : \m p1 p2 -> case m of
    Just s -> ClickData
      { lines : M.singleton (Line p1 p2) (S.singleton s)
      , circles : Nothing
      , corners : M.fromFoldable $ (\k -> Tuple k $ S.singleton s) <$>
        [p1, p2]
      }
    Nothing -> mempty
  , drawCirc : \s c r -> ClickData
    { lines : M.empty
    , circles : Just { center : c, radius : r, segment : s }
    , corners : M.fromFoldable $ (\k -> Tuple k $ S.singleton s) <$>
      [ c <> Point2D (Horizontal r) mempty
      , c <> Point2D (Horizontal $ negate r) mempty
      , c <> Point2D mempty (Vertical r)
      , c <> Point2D mempty (Vertical $ negate r)
      ]
    }
  }

data ClickEvent
  = Hover (Set Segment)
  | Unhover (Set Segment)
  | Click (Set Segment)

renderClickData :: ClickData -> Drawing ClickEvent
renderClickData (ClickData clickdata) = withoutViewport $
  drawnLines <> drawnCirc <> drawnCorners
  -- Corners are last so clicking them is prioritised
  where
  drawnLines = foldMapWithIndex drawLine clickdata.lines
  drawnCirc = foldMap drawCirc clickdata.circles
  drawnCorners = foldMapWithIndex drawCorner clickdata.corners
  drawLine (Line p1 p2) s = line p1 p2 transparent $ props s
  drawCirc circ =
    circle circ.center circ.radius transparent NoColor $
    props $ S.singleton circ.segment
  drawCorner point s =
    if S.size s == 1 then mempty else
    circle point 1.0 NoColor transparent $ props s
  props segment = DrawingProps
    [ HP.prop (PropName "pointerEvents") "all"
    , HE.onMouseEnter $ pure $ Hover segment
    , HE.onMouseOut $ pure $ Unhover segment
    , HE.onClick $ pure $ Click segment
    ]

clickArea :: Drawing ClickEvent
clickArea = renderClickData $ drawGlyph drawClickData fullGlyph
