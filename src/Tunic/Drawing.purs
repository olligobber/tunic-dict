module Tunic.Drawing
  ( drawGlyph
  , drawWord
) where

import Prelude ((<$>), (<*>), (<>), (>>>), ($), (/))
import Data.Array (mapWithIndex)
import Data.Foldable (fold, foldMap)
import Data.Int (toNumber)

import Drawing
  ( Drawing, WSO, Point2D
  , point2D, scaled, rescale, moveOffset, line, circle
  )
import Tunic.Glyph
  ( Segment(..), ConsonantSegment(..), VowelSegment(..), CircSegment(..)
  , Glyph, Word
  )

topBackCorner :: WSO Point2D
topBackCorner = point2D 7.0 1.0

topLeftCorner :: WSO Point2D
topLeftCorner = point2D 1.0 5.0

topRightCorner :: WSO Point2D
topRightCorner = point2D 13.0 5.0

topFrontCorner :: WSO Point2D
topFrontCorner = point2D 7.0 9.0

bottomBackCorner :: WSO Point2D
bottomBackCorner = point2D 7.0 13.0

bottomLeftCorner :: WSO Point2D
bottomLeftCorner = point2D 1.0 17.0

bottomRightCorner :: WSO Point2D
bottomRightCorner = point2D 13.0 17.0

bottomFrontCorner :: WSO Point2D
bottomFrontCorner = point2D 7.0 21.0

leftEdgeAbove :: WSO Point2D
leftEdgeAbove = point2D 1.0 11.0

leftEdgeBelow :: WSO Point2D
leftEdgeBelow = point2D 1.0 13.0

middleEdgeAbove :: WSO Point2D
middleEdgeAbove = point2D 7.0 11.0

middleEdgeBelow :: WSO Point2D
middleEdgeBelow = point2D 7.0 13.0

rightEdgeAbove :: WSO Point2D
rightEdgeAbove = point2D 13.0 11.0

circCenter :: WSO Point2D
circCenter = point2D 7.0 22.0

strokeWidth :: WSO Number
strokeWidth = scaled 1.0

circleRadius :: WSO Number
circleRadius = scaled 1.0

drawSegment :: Segment -> WSO Drawing
drawSegment (Vowel TopRight) =
  line <$> topBackCorner <*> topRightCorner <*> strokeWidth
drawSegment (Vowel BottomRight) =
  line <$> bottomRightCorner <*> bottomFrontCorner <*> strokeWidth
drawSegment (Vowel BottomLeft) =
  line <$> bottomFrontCorner <*> bottomLeftCorner <*> strokeWidth
drawSegment (Vowel Left) =
  (line <$> topLeftCorner <*> leftEdgeAbove <*> strokeWidth) <>
  (line <$> leftEdgeBelow <*> bottomLeftCorner <*> strokeWidth)
drawSegment (Vowel TopLeft) =
  line <$> topBackCorner <*> topLeftCorner <*> strokeWidth
drawSegment (Consonant Up) =
  line <$> topBackCorner <*> middleEdgeAbove <*> strokeWidth
drawSegment (Consonant UpRight) =
  line <$> topFrontCorner <*> topRightCorner <*> strokeWidth
drawSegment (Consonant DownRight) =
  line <$> bottomBackCorner <*> bottomRightCorner <*> strokeWidth
drawSegment (Consonant Down) =
  (line <$> topFrontCorner <*> middleEdgeAbove <*> strokeWidth) <>
  (line <$> middleEdgeBelow <*> bottomFrontCorner <*> strokeWidth)
drawSegment (Consonant DownLeft) =
  line <$> bottomBackCorner <*> bottomLeftCorner <*> strokeWidth
drawSegment (Consonant UpLeft) =
  line <$> topFrontCorner <*> topLeftCorner<*> strokeWidth
drawSegment (Circ CircSegment) =
  circle <$> circCenter <*> circleRadius <*> strokeWidth

drawGlyph :: Glyph -> WSO Drawing
drawGlyph glyph = rescale (1.0 / 24.0) $
  foldMap drawSegment glyph <>
  (line <$> leftEdgeAbove <*> rightEdgeAbove <*> scaled 1.0)

drawWord :: Word -> WSO Drawing
drawWord = mapWithIndex drawWithIndex >>> fold
  where
  drawWithIndex i g = moveOffset { x : toNumber i / 2.0, y : 0.0 } $ drawGlyph g