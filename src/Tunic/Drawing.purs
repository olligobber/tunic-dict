module Tunic.Drawing
  ( drawGlyph
  , drawWord
) where

import Prelude ((<$>), (<*>), (<>), (>>>), ($), (*), mempty, pure)
import Data.Array (mapWithIndex)
import Data.Foldable (fold, foldMap)
import Data.Int (toNumber)

import Drawing
  ( Drawing, Offset, Vertical(..), Horizontal(..), Point2D(..)
  , point2D, moveOffset, line, circle
  )
import Tunic.Glyph
  ( Segment(..), ConsonantSegment(..), VowelSegment(..), CircSegment(..)
  , Glyph, Word
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

strokeWidth :: Offset Number
strokeWidth = pure 1.0

circleRadius :: Offset Number
circleRadius = pure 1.0

drawSegment :: Segment -> Offset Drawing
drawSegment (Vowel TopRight) =
  line <$> point2D middleX topCornerY <*> point2D rightX topFaceY <*> strokeWidth
drawSegment (Vowel BottomRight) =
  line <$> point2D rightX bottomFaceY <*> point2D middleX bottomCornerY <*> strokeWidth
drawSegment (Vowel BottomLeft) =
  line <$> point2D middleX bottomCornerY <*> point2D leftX bottomFaceY <*> strokeWidth
drawSegment (Vowel Left) =
  (line <$> point2D leftX topFaceY <*> point2D leftX lineY <*> strokeWidth) <>
  (line <$> point2D leftX belowLineY <*> point2D leftX bottomFaceY <*> strokeWidth)
drawSegment (Vowel TopLeft) =
  line <$> point2D middleX topCornerY <*> point2D leftX topFaceY <*> strokeWidth
drawSegment (Consonant Up) =
  line <$> point2D middleX topCornerY <*> point2D middleX lineY <*> strokeWidth
drawSegment (Consonant UpRight) =
  line <$> point2D middleX frontCornerY <*> point2D rightX topFaceY <*> strokeWidth
drawSegment (Consonant DownRight) =
  line <$> point2D middleX belowLineY <*> point2D rightX bottomFaceY <*> strokeWidth
drawSegment (Consonant Down) =
  (line <$> point2D middleX frontCornerY <*> point2D middleX lineY <*> strokeWidth) <>
  (line <$> point2D middleX belowLineY <*> point2D middleX bottomCornerY <*> strokeWidth)
drawSegment (Consonant DownLeft) =
  line <$> point2D middleX belowLineY <*> point2D leftX bottomFaceY <*> strokeWidth
drawSegment (Consonant UpLeft) =
  line <$> point2D middleX frontCornerY <*> point2D leftX topFaceY<*> strokeWidth
drawSegment (Circ CircSegment) =
  circle <$> point2D middleX circleCenterY <*> circleRadius <*> strokeWidth

drawGlyph :: Glyph -> Offset Drawing
drawGlyph glyph =
  foldMap drawSegment glyph <>
  (line <$> point2D leftX lineY <*> point2D rightX lineY <*> strokeWidth)

drawWord :: Word -> Drawing
drawWord = mapWithIndex drawWithIndex >>> fold >>> (_ $ mempty)
  where
  drawWithIndex i g = moveOffset
    (Point2D (Horizontal $ 12.0 * toNumber i) mempty)
    (drawGlyph g)