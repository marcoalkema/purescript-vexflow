module Main where

import Prelude
-- import Control.Monad.Eff
-- import Control.Monad.Eff.Console
import VexFlow as Vx

main :: Vx.VexFlowEff
main = do
  canvas <- Vx.createCanvas "notationCanvas"
  renderer <- Vx.createRenderer canvas
  ctx <- Vx.createCtx renderer
  stave <- Vx.createStave 1.0 1.0 500.0
  Vx.drawStave stave "treble" ctx
  notes <- Vx.createNote ["c/4", "e/4", "g/4"] "w"
  voice <- Vx.createNewVoice 1.0 1.0
  voices <- Vx.addNotesToVoice notes voice
  Vx.formatter voice 500.0
  Vx.drawVoice ctx stave
  
