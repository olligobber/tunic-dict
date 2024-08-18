module Main where

import Prelude (Unit, unit, pure, ($), (>>=))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI component unit

component :: forall q m. H.Component q Unit Unit m
component = H.mkComponent 
  { initialState : pure unit 
  , render : pure $ HH.text "test"
  , eval : H.mkEval $ H.defaultEval
  }