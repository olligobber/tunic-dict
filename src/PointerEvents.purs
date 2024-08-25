module PointerEvents
  ( SVGline
  , SVGcircle
  , line
  , circle
) where

import Halogen.HTML (Leaf)
import Halogen.Svg.Elements as SE
import Halogen.Svg.Indexed as SI
import Unsafe.Coerce (unsafeCoerce)

type SVGline :: Row Type
type SVGline = (pointerEvents :: String | SI.SVGline)

type SVGcircle :: Row Type
type SVGcircle = (pointerEvents :: String | SI.SVGcircle)

line :: forall p i. Leaf SVGline p i
line = unsafeCoerce SE.line

circle :: forall p i. Leaf SVGcircle p i
circle = unsafeCoerce SE.circle