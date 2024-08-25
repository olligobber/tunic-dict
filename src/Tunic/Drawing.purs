module Tunic.Drawing
  ( drawGlyph
  , drawWord
  , DrawFunc
  , simpleDraw
  , simpleDrawNoLine
) where

import Prelude
  ( (<$>), (<*>), (<>), (>>>), ($), (*), mempty
  , class Semigroup, class Monoid
  )
import Data.Array (mapWithIndex)
import Data.Foldable (class Foldable, fold, foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Halogen.Svg.Attributes (Color(Named, NoColor))

import Drawing
  ( Drawing, Offset, Vertical(..), Horizontal(..), Point2D(..), DrawingProps(..)
  , withOffset, moveOffset, line, circle
  )
import Tunic.Glyph
  ( Segment(..), ConsonantSegment(..), VowelSegment(..), CircSegment(..)
  , Word
  )

leftX :: Horizontal
leftX = Horizontal 0.0

middleX :: Horizontal
middleX = Horizontal 6.0

rightX :: Horizontal
rightX = Horizontal 12.0

topCornerY :: Vertical
topCornerY = Vertical 0.0

topFaceY :: Vertical
topFaceY = Vertical 4.0

frontCornerY :: Vertical
frontCornerY = Vertical 8.0

lineY :: Vertical
lineY = Vertical 10.0

belowLineY :: Vertical
belowLineY = Vertical 12.0

bottomFaceY :: Vertical
bottomFaceY = Vertical 16.0

bottomCornerY :: Vertical
bottomCornerY = Vertical 20.0

circleCenterY :: Vertical
circleCenterY = Vertical 21.0

circleRadius :: Number
circleRadius = 1.0

type DrawFunc m =
  { drawLine :: Maybe Segment -> Point2D -> Point2D -> m
  , drawCirc :: Segment -> Point2D -> Number -> m
  }

simpleDraw :: forall i. Color -> DrawFunc (Offset (Drawing i))
simpleDraw color =
  { drawLine: \_ p1 p2 ->
      (\a b -> line a b color (DrawingProps [])) <$>
      withOffset p1 <*>
      withOffset p2
  , drawCirc: \_ p r ->
      (\a -> circle a r color NoColor (DrawingProps [])) <$>
      withOffset p
  }

simpleDrawNoLine :: forall i. Color -> DrawFunc (Offset (Drawing i))
simpleDrawNoLine color =
  { drawLine: \m p1 p2 -> case m of
      Nothing -> mempty
      Just _ -> (\a b -> line a b color (DrawingProps [])) <$>
        withOffset p1 <*>
        withOffset p2
  , drawCirc: \_ p r ->
      (\a -> circle a r color NoColor (DrawingProps [])) <$>
      withOffset p
  }

drawSegment :: forall m. Semigroup m => DrawFunc m -> Segment -> m
drawSegment { drawLine } s@(Vowel TopRight) =
  drawLine (Just s) (Point2D middleX topCornerY) (Point2D rightX topFaceY)
drawSegment { drawLine } s@(Vowel BottomRight) =
  drawLine (Just s) (Point2D rightX bottomFaceY) (Point2D middleX bottomCornerY)
drawSegment { drawLine } s@(Vowel BottomLeft) =
  drawLine (Just s) (Point2D middleX bottomCornerY) (Point2D leftX bottomFaceY)
drawSegment { drawLine } s@(Vowel Left) =
  drawLine (Just s) (Point2D leftX topFaceY) (Point2D leftX lineY) <>
  drawLine (Just s) (Point2D leftX belowLineY) (Point2D leftX bottomFaceY)
drawSegment { drawLine } s@(Vowel TopLeft) =
  drawLine (Just s) (Point2D middleX topCornerY) (Point2D leftX topFaceY)
drawSegment { drawLine } s@(Consonant Up) =
  drawLine (Just s) (Point2D middleX topCornerY) (Point2D middleX frontCornerY) <>
  drawLine (Just s) (Point2D middleX frontCornerY) (Point2D middleX lineY)
drawSegment { drawLine } s@(Consonant UpRight) =
  drawLine (Just s) (Point2D middleX frontCornerY) (Point2D rightX topFaceY)
drawSegment { drawLine } s@(Consonant DownRight) =
  drawLine (Just s) (Point2D middleX belowLineY) (Point2D rightX bottomFaceY)
drawSegment { drawLine } s@(Consonant Down) =
  drawLine (Just s) (Point2D middleX frontCornerY) (Point2D middleX lineY) <>
  drawLine (Just s) (Point2D middleX belowLineY) (Point2D middleX bottomCornerY)
drawSegment { drawLine } s@(Consonant DownLeft) =
  drawLine (Just s) (Point2D middleX belowLineY) (Point2D leftX bottomFaceY)
drawSegment { drawLine } s@(Consonant UpLeft) =
  drawLine (Just s) (Point2D middleX frontCornerY) (Point2D leftX topFaceY)
drawSegment { drawCirc } s@(Circ CircSegment) =
  drawCirc s (Point2D middleX circleCenterY) circleRadius

drawGlyph :: forall f m. Foldable f => Monoid m =>
  DrawFunc m -> f Segment -> m
drawGlyph drawFunc glyph =
  foldMap (drawSegment drawFunc) glyph <>
  drawFunc.drawLine Nothing (Point2D leftX lineY) (Point2D rightX lineY)

drawWord :: forall i. Word -> Drawing i
drawWord = mapWithIndex drawWithIndex >>> fold >>> (_ $ mempty)
  where
  drawWithIndex i g = moveOffset
    (Point2D (Horizontal $ 12.0 * toNumber i) mempty)
    (drawGlyph (simpleDraw $ Named "black") g)