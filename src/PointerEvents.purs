module PointerEvents
  ( SVGline
  , SVGcircle
  , SVGg
  , line
  , circle
  , g
) where

import Halogen.HTML (Leaf, Node)
import Halogen.Svg.Elements as SE
import Halogen.Svg.Indexed as SI
import Unsafe.Coerce (unsafeCoerce)

type SVGline :: Row Type
type SVGline = (pointerEvents :: String | SI.SVGline)

type SVGcircle :: Row Type
type SVGcircle = (pointerEvents :: String | SI.SVGcircle)

type SVGg :: Row Type
type SVGg = (pointerEvents :: String | SI.SVGg)

line :: forall p i. Leaf SVGline p i
line = unsafeCoerce SE.line

circle :: forall p i. Leaf SVGcircle p i
circle = unsafeCoerce SE.circle

g :: forall p i. Node SVGg p i
g = unsafeCoerce SE.g