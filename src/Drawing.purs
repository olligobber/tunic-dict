module Drawing
  ( Drawing
  , Horizontal(..)
  , Vertical(..)
  , Point2D
  , Offset
  , WO
  , point2D
  , moveOffset
  , line
  , circle
  , renderDrawing
  , simpleOffset
) where

import Prelude
  ( ($), (<>), (+), (*), (/), (-), map, min, max, negate, mempty
  , class Eq, class Ord, class Semigroup, class Monoid
  )
import Data.Group (class Group, ginverse)
import Data.Maybe (Maybe(..))
import Data.Ord.Min (Min(..))
import Data.Ord.Max (Max(..))
import Halogen.HTML (PlainHTML, fromPlainHTML, Leaf)
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(LineCapRound))
import Halogen.Svg.Indexed (SVGsvg)

gsubtract :: forall g. Group g => g -> g -> g
gsubtract a b = a <> ginverse b
infixr 6 gsubtract as </>

newtype Horizontal = Horizontal Number
fromHorizontal :: Horizontal -> Number
fromHorizontal (Horizontal x) = x

derive instance eqHorizontal :: Eq Horizontal
derive instance ordHorizontal :: Ord Horizontal

instance semigroupHorizontal :: Semigroup Horizontal where
  append (Horizontal a) (Horizontal b) = Horizontal $ a + b

instance monoidHorizontal :: Monoid Horizontal where
  mempty = Horizontal 0.0

instance groupHorizontal :: Group Horizontal where
  ginverse (Horizontal x) = Horizontal $ negate x

newtype Vertical = Vertical Number
fromVertical :: Vertical -> Number
fromVertical (Vertical x) = x

derive instance eqVertical :: Eq Vertical
derive instance ordVertical :: Ord Vertical

instance semigroupVertical :: Semigroup Vertical where
  append (Vertical a) (Vertical b) = Vertical $ a + b

instance monoidVertical :: Monoid Vertical where
  mempty = Vertical 0.0

instance groupVertical :: Group Vertical where
  ginverse (Vertical x) = Vertical $ negate x

data Drawing = Drawing
  { elements :: Array PlainHTML
  , minX :: Maybe (Min Horizontal)
  , maxX :: Maybe (Max Horizontal)
  , minY :: Maybe (Min Vertical)
  , maxY :: Maybe (Max Vertical)
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

type Point2D = { x :: Horizontal, y :: Vertical }

type Offset = { x :: Horizontal, y :: Vertical }

type WO x = Offset -> x

point2D :: Horizontal -> Vertical -> WO Point2D
point2D x y offset =
  { x : offset.x <> x
  , y : offset.y <> y
  }

moveOffset :: forall x. Offset -> WO x -> WO x
moveOffset p f offset = f $ { x : offset.x <> p.x, y : offset.y <> p.y }

line :: Point2D -> Point2D -> Number -> Drawing
line p1 p2 width = Drawing
  { elements: [SE.line
    [ SA.x1 $ fromHorizontal p1.x
    , SA.x2 $ fromHorizontal p2.x
    , SA.y1 $ fromVertical p1.y
    , SA.y2 $ fromVertical p2.y
    , SA.stroke $ SA.Named "black"
    , SA.strokeWidth width
    , SA.strokeLineCap LineCapRound
    ]]
  , minX : Just $ Min $ min p1.x p2.x </> Horizontal width
  , maxX : Just $ Max $ max p1.x p2.x <> Horizontal width
  , minY : Just $ Min $ min p1.y p2.y </> Vertical width
  , maxY : Just $ Max $ max p1.y p2.y <> Vertical width
  }


circle :: Point2D -> Number -> Number -> Drawing
circle center radius width = Drawing
  { elements : [SE.circle
    [ SA.cx $ fromHorizontal center.x
    , SA.cy $ fromVertical center.y
    , SA.r radius
    , SA.stroke $ SA.Named "black"
    , SA.strokeWidth width
    , SA.fill SA.NoColor
    ]]
  , minX : Just $ Min $ center.x </> Horizontal (radius + width)
  , maxX : Just $ Max $ center.x <> Horizontal (radius + width)
  , minY : Just $ Min $ center.y </> Vertical (radius + width)
  , maxY : Just $ Max $ center.y <> Vertical (radius + width)
  }

renderDrawing :: forall w i. Drawing -> Leaf SVGsvg w i
renderDrawing (Drawing d) attrs = SE.svg
  ([SA.viewBox minX minY width height] <> attrs)
  (map fromPlainHTML d.elements)
  where
  width = fromHorizontal $ difference {min: d.minX, max: d.maxX}
  height = fromVertical $ difference {min: d.minY, max: d.maxY}
  difference :: forall t. Group t => Ord t =>
    {min :: Maybe (Min t), max :: Maybe (Max t)} -> t
  difference {min: Just (Min a), max: (Just (Max b))} = max mempty $ b </> a
  difference _ = mempty
  minX = case d.minX of
    Nothing -> 0.0
    Just (Min (Horizontal x)) -> x
  minY = case d.minY of
    Nothing -> 0.0
    Just (Min (Vertical y)) -> y

simpleOffset :: Offset
simpleOffset = {x : mempty, y : mempty}