module Drawing
  ( Drawing
  , Horizontal(..)
  , Vertical(..)
  , Point2D(..)
  , Offset
  , point2D
  , moveOffset
  , line
  , circle
  , renderDrawing
) where

import Prelude
  ( ($), (<>), (+), map, min, max, negate, mempty
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

data Point2D = Point2D Horizontal Vertical

instance semigroupPoint2D :: Semigroup Point2D where
  append (Point2D x1 y1) (Point2D x2 y2) = Point2D (x1 <> x2) (y1 <> y2)

instance monoidPoint2D :: Monoid Point2D where
  mempty = Point2D mempty mempty

instance groupPoint2D :: Group Point2D where
  ginverse (Point2D x y) = Point2D (ginverse x) (ginverse y)

type Offset x = Point2D -> x

point2D :: Horizontal -> Vertical -> Offset Point2D
point2D x y offset = offset <> Point2D x y

moveOffset :: forall x. Point2D -> Offset x -> Offset x
moveOffset p f offset = f $ offset <> p

line :: Point2D -> Point2D -> Number -> Drawing
line (Point2D x1 y1) (Point2D x2 y2) width = Drawing
  { elements: [SE.line
    [ SA.x1 $ fromHorizontal x1
    , SA.x2 $ fromHorizontal x2
    , SA.y1 $ fromVertical y1
    , SA.y2 $ fromVertical y2
    , SA.stroke $ SA.Named "black"
    , SA.strokeWidth width
    , SA.strokeLineCap LineCapRound
    ]]
  , minX : Just $ Min $ min x1 x2 </> Horizontal width
  , maxX : Just $ Max $ max x1 x2 <> Horizontal width
  , minY : Just $ Min $ min y1 y2 </> Vertical width
  , maxY : Just $ Max $ max y1 y2 <> Vertical width
  }


circle :: Point2D -> Number -> Number -> Drawing
circle (Point2D x y) radius width = Drawing
  { elements : [SE.circle
    [ SA.cx $ fromHorizontal x
    , SA.cy $ fromVertical y
    , SA.r radius
    , SA.stroke $ SA.Named "black"
    , SA.strokeWidth width
    , SA.fill SA.NoColor
    ]]
  , minX : Just $ Min $ x </> Horizontal fullRadius
  , maxX : Just $ Max $ x <> Horizontal fullRadius
  , minY : Just $ Min $ y </> Vertical fullRadius
  , maxY : Just $ Max $ y <> Vertical fullRadius
  }
  where
  fullRadius = radius + width

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