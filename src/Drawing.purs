module Drawing
  ( Drawing
  , Point2D
  , ScaleOffset
  , WSO
  , point2D
  , scaled
  , moveOffset
  , rescale
  , line
  , circle
  , renderDrawing
  , simpleScaleOffset
) where

import Prelude
  ( ($), (<>), (+), (*), (/), (-), map, min, max
  , class Semigroup, class Monoid
  )
import Data.Maybe (Maybe(..))
import Data.Ord.Min (Min(..))
import Data.Ord.Max (Max(..))
import Halogen.HTML (PlainHTML, fromPlainHTML, Leaf)
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(LineCapRound))
import Halogen.Svg.Indexed (SVGsvg)

data Drawing = Drawing
  { elements :: Array PlainHTML
  , minX :: Maybe (Min Number)
  , maxX :: Maybe (Max Number)
  , minY :: Maybe (Min Number)
  , maxY :: Maybe (Max Number)
  }

instance semigroupDrawing :: Semigroup Drawing where
  append (Drawing d1) (Drawing d2) = Drawing $
    { elements : d1.elements <> d2.elements
    , minX : d1.minX <> d2.minX
    , maxX : d1.maxX <> d2.maxX
    , minY : d1.minY <> d2.minY
    , maxY : d1.maxY <> d2.maxY
    }

instance monoidDrawing :: Monoid Drawing where
  mempty = Drawing
    { elements: []
    , minX: Nothing
    , maxX : Nothing
    , minY : Nothing
    , maxY : Nothing
    }

type Point2D = { x :: Number, y :: Number }

type ScaleOffset = { x :: Number, y :: Number, s :: Number }

type WSO x = ScaleOffset -> x

point2D :: Number -> Number -> WSO Point2D
point2D x y os =
  { x : os.s * (os.x + x)
  , y : os.s * (os.y + y)
  }

scaled :: Number -> WSO Number
scaled n os = os.s * n

moveOffset :: forall x. Point2D -> WSO x -> WSO x
moveOffset p f os = f $ os { x = (os.x + p.x), y = (os.y + p.y) }

rescale :: forall x. Number -> WSO x -> WSO x
rescale s f os = f
  { x : os.x / s
  , y : os.y / s
  , s : s * os.s
  }

line :: Point2D -> Point2D -> Number -> Drawing
line p1 p2 width = Drawing
  { elements: [SE.line
    [ SA.x1 p1.x
    , SA.x2 p2.x
    , SA.y1 p1.y
    , SA.y2 p2.y
    , SA.stroke $ SA.Named "black"
    , SA.strokeWidth width
    , SA.strokeLineCap LineCapRound
    ]]
  , minX : Just $ Min $ min p1.x p2.x - width
  , maxX : Just $ Max $ max p1.x p2.x + width
  , minY : Just $ Min $ min p1.y p2.y - width
  , maxY : Just $ Max $ max p1.y p2.y + width
  }


circle :: Point2D -> Number -> Number -> Drawing
circle center radius width = Drawing
  { elements : [SE.circle
    [ SA.cx center.x
    , SA.cy center.y
    , SA.r radius
    , SA.stroke $ SA.Named "black"
    , SA.strokeWidth width
    , SA.fill SA.NoColor
    ]]
  , minX : Just $ Min $ center.x - radius - width
  , maxX : Just $ Max $ center.x + radius + width
  , minY : Just $ Min $ center.y - radius - width
  , maxY : Just $ Max $ center.y + radius + width
  }

renderDrawing :: forall w i. Drawing -> Leaf SVGsvg w i
renderDrawing (Drawing d) attrs = SE.svg
  (
    [ SA.viewBox minX minY width height
    , SA.preserveAspectRatio (Just {x_ : SA.Min, y_ : SA.Min}) SA.Meet
    , SA.width width
    , SA.height height
    ] <> attrs
  )
  (map fromPlainHTML d.elements)
  where
  width = difference {min: d.minX, max: d.maxX}
  height = difference {min: d.minY, max: d.maxY}
  difference {min: Just (Min a), max: (Just (Max b))} = max 0.0 $ b - a
  difference _ = 0.0
  minX = case d.minX of
    Nothing -> 0.0
    Just (Min x) -> x
  minY = case d.minY of
    Nothing -> 0.0
    Just (Min y) -> y

simpleScaleOffset :: ScaleOffset
simpleScaleOffset = {x : 0.0, y : 0.0, s : 1.0}