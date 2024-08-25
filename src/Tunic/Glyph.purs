module Tunic.Glyph
  ( VowelSegment(..)
  , ConsonantSegment(..)
  , CircSegment(..)
  , Segment(..)
  , encodeSegment
  , decodeSegment
  , Glyph
  , fullGlyph
  , VowelPart
  , ConsonantPart
  , Sound
  , encodeGlyph
  , decodeGlyph
  , fromVowel
  , fromConsonant
  , fromSound
  , getParts
  , Word
) where

import Prelude (class Eq, class Ord, ($), pure, (<>), show, (>>>), map, bind)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe(..))
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as Set
import Data.Set (fromFoldable)
import Data.String.CodeUnits as String
import Data.Traversable (traverse)

data VowelSegment
  = TopRight
  | BottomRight
  | BottomLeft
  | Left
  | TopLeft

derive instance eqVowelSegment :: Eq VowelSegment
derive instance ordVowelSegment :: Ord VowelSegment

data ConsonantSegment
  = Up
  | UpRight
  | DownRight
  | Down
  | DownLeft
  | UpLeft

derive instance eqConsonantSegment :: Eq ConsonantSegment
derive instance ordConsonantSegment :: Ord ConsonantSegment

data CircSegment
  = CircSegment

derive instance eqCircSegment :: Eq CircSegment
derive instance ordCircSegment :: Ord CircSegment

data Segment
  = Vowel VowelSegment
  | Consonant ConsonantSegment
  | Circ CircSegment

derive instance eqSegment :: Eq Segment
derive instance ordSegment :: Ord Segment

encodeSegment :: Segment -> Char
encodeSegment (Vowel TopRight) = '1'
encodeSegment (Vowel BottomRight) = '5'
encodeSegment (Vowel BottomLeft) = '7'
encodeSegment (Vowel Left) = '9'
encodeSegment (Vowel TopLeft) = 'B'
encodeSegment (Consonant Up) = '0'
encodeSegment (Consonant UpRight) = '2'
encodeSegment (Consonant DownRight) = '4'
encodeSegment (Consonant Down) = '6'
encodeSegment (Consonant DownLeft) = '8'
encodeSegment (Consonant UpLeft) = 'A'
encodeSegment (Circ CircSegment) = 'C'

decodeSegment :: forall m. MonadThrow String m => Char -> m Segment
decodeSegment '0' = pure $ Consonant Up
decodeSegment '1' = pure $ Vowel TopRight
decodeSegment '2' = pure $ Consonant UpRight
decodeSegment '4' = pure $ Consonant DownRight
decodeSegment '5' = pure $ Vowel BottomRight
decodeSegment '6' = pure $ Consonant Down
decodeSegment '7' = pure $ Vowel BottomLeft
decodeSegment '8' = pure $ Consonant DownLeft
decodeSegment '9' = pure $ Vowel Left
decodeSegment 'A' = pure $ Consonant UpLeft
decodeSegment 'B' = pure $ Vowel TopLeft
decodeSegment 'C' = pure $ Circ CircSegment
decodeSegment char = throwError $ "Unknown segment: " <> show char

type Glyph = NonEmptySet Segment

fullGlyph :: Glyph
fullGlyph = Set.cons (Circ CircSegment) $ fromFoldable
  [ Vowel TopRight
  , Vowel BottomRight
  , Vowel BottomLeft
  , Vowel Left
  , Vowel TopLeft
  , Consonant Up
  , Consonant UpRight
  , Consonant DownRight
  , Consonant Down
  , Consonant DownLeft
  , Consonant UpLeft
  ]

type VowelPart = NonEmptySet VowelSegment

type ConsonantPart = NonEmptySet ConsonantSegment

data Sound
  = VowelSound VowelPart
  | ConsonantSound ConsonantPart

encodeGlyph :: forall f. Foldable f => f Segment -> String
encodeGlyph = foldMap (encodeSegment >>> pure) >>> String.fromCharArray

decodeGlyph :: forall m. MonadThrow String m => String -> m Glyph
decodeGlyph string = do
  decodedArray <- traverse decodeSegment $ String.toCharArray string
  case Set.fromFoldable decodedArray of
    Nothing -> throwError "Glyph cannot be empty"
    Just s -> pure s

fromVowel :: VowelPart -> Glyph
fromVowel = Set.map Vowel

fromConsonant :: ConsonantPart -> Glyph
fromConsonant = Set.map Consonant

fromSound :: Sound -> Glyph
fromSound (VowelSound vowel) = fromVowel vowel
fromSound (ConsonantSound consonant) = fromConsonant consonant

getParts :: Glyph ->
  { vowelPart :: Maybe VowelPart
  , consonantPart :: Maybe ConsonantPart
  , circPart :: Maybe CircSegment
  }
getParts glyph =
  { vowelPart: Set.fromSet $ Set.mapMaybe getVowel glyph
  , consonantPart: Set.fromSet $ Set.mapMaybe getConsonant glyph
  , circPart: map Set.min $ Set.fromSet $ Set.mapMaybe getCirc glyph
  }
  where
  getVowel (Vowel segment) = Just segment
  getVowel _ = Nothing
  getConsonant (Consonant segment) = Just segment
  getConsonant _ = Nothing
  getCirc (Circ segment) = Just segment
  getCirc _ = Nothing

type Word = Array Glyph