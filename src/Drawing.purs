module Drawing
  ( DrawingProps(..)
  , Drawing
  , Horizontal(..)
  , fromHorizontal
  , Vertical(..)
  , fromVertical
  , Point2D(..)
  , Offset
  , withOffset
  , point2D
  , moveOffset
  , line
  , circle
  , withoutViewport
  , renderDrawing
) where

import Prelude
  ( ($), (<>), (+), (&&), (==), min, max, negate, mempty, compare
  , class Eq, class Ord, class Semigroup, class Monoid
  )
import Data.Group (class Group, ginverse)
import Data.Maybe (Maybe(..))
import Data.Ord.Min (Min(..))
import Data.Ord.Max (Max(..))
import Halogen.HTML (HTML, Leaf, IProp)
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes (Color)
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(LineCapRound))
import Halogen.Svg.Indexed (SVGsvg, GlobalAttributes)

import PointerEvents as U

-- TODO replace all floats with ints and only cast to float at the end

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

newtype DrawingProps i = DrawingProps
  -- (forall r. Array (IProp (GlobalAttributes r) i))
  (forall r. Array (IProp (GlobalAttributes (pointerEvents :: String | r)) i))

data Drawing i = Drawing
  { elements :: forall w. Array (HTML w i)
  , minX :: Maybe (Min Horizontal)
  , maxX :: Maybe (Max Horizontal)
  , minY :: Maybe (Min Vertical)
  , maxY :: Maybe (Max Vertical)
  }

instance semigroupDrawing :: Semigroup (Drawing i) where
  append (Drawing d1) (Drawing d2) = Drawing $
    { elements : d1.elements <> d2.elements
    , minX : d1.minX <> d2.minX
    , maxX : d1.maxX <> d2.maxX
    , minY : d1.minY <> d2.minY
    , maxY : d1.maxY <> d2.maxY
    }

instance monoidDrawing :: Monoid (Drawing i) where
  mempty = Drawing
    { elements: []
    , minX: Nothing
    , maxX : Nothing
    , minY : Nothing
    , maxY : Nothing
    }

data Point2D = Point2D Horizontal Vertical

-- Eq and Ord instances mainly for maps, note they use floats so expect errors
-- if too much arithmetic is done
instance eqPoint2D :: Eq Point2D where
  eq
    (Point2D (Horizontal x1) (Vertical y1))
    (Point2D (Horizontal x2) (Vertical y2))
    =
    (x1 == x2) && (y1 == y2)

instance ordPoint2D :: Ord Point2D where
  compare
    (Point2D (Horizontal x1) (Vertical y1))
    (Point2D (Horizontal x2) (Vertical y2))
    =
    compare [x1, y1] [x2, y2]

instance semigroupPoint2D :: Semigroup Point2D where
  append (Point2D x1 y1) (Point2D x2 y2) = Point2D (x1 <> x2) (y1 <> y2)

instance monoidPoint2D :: Monoid Point2D where
  mempty = Point2D mempty mempty

instance groupPoint2D :: Group Point2D where
  ginverse (Point2D x y) = Point2D (ginverse x) (ginverse y)

type Offset x = Point2D -> x

withOffset :: Point2D -> Offset Point2D
withOffset p offset = p <> offset

point2D :: Horizontal -> Vertical -> Offset Point2D
point2D x y offset = offset <> Point2D x y

moveOffset :: forall x. Point2D -> Offset x -> Offset x
moveOffset p f offset = f $ offset <> p

line :: forall i.
  Point2D -> Point2D -> Color -> DrawingProps i -> Drawing i
line (Point2D x1 y1) (Point2D x2 y2) color (DrawingProps props) = Drawing
  -- { elements: [SE.line $
  { elements: [U.line $
    [ SA.x1 $ fromHorizontal x1
    , SA.x2 $ fromHorizontal x2
    , SA.y1 $ fromVertical y1
    , SA.y2 $ fromVertical y2
    , SA.stroke color
    , SA.strokeWidth 1.0
    , SA.strokeLineCap LineCapRound
    ] <> props]
  , minX : Just $ Min $ min x1 x2 </> Horizontal 1.0
  , maxX : Just $ Max $ max x1 x2 <> Horizontal 1.0
  , minY : Just $ Min $ min y1 y2 </> Vertical 1.0
  , maxY : Just $ Max $ max y1 y2 <> Vertical 1.0
  }

circle :: forall i.
  Point2D -> Number -> Color -> Color -> DrawingProps i -> Drawing i
circle (Point2D x y) radius stroke fill (DrawingProps props) = Drawing
  -- { elements: [SE.circle $
  { elements : [U.circle $
    [ SA.cx $ fromHorizontal x
    , SA.cy $ fromVertical y
    , SA.r radius
    , SA.stroke stroke
    , SA.strokeWidth 1.0
    , SA.fill fill
    ] <> props]
  , minX : Just $ Min $ x </> Horizontal fullRadius
  , maxX : Just $ Max $ x <> Horizontal fullRadius
  , minY : Just $ Min $ y </> Vertical fullRadius
  , maxY : Just $ Max $ y <> Vertical fullRadius
  }
  where
  fullRadius = radius + 1.0

withoutViewport :: forall i. Drawing i -> Drawing i
withoutViewport (Drawing d) = Drawing $ d
  { minX = Nothing
  , maxX = Nothing
  , minY = Nothing
  , maxY = Nothing
  }

renderDrawing :: forall w i. Drawing i -> Leaf SVGsvg w i
renderDrawing (Drawing d) attrs = SE.svg
  ([SA.viewBox minX minY width height] <> attrs)
  d.elements
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