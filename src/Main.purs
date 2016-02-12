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
  Vx.drawStave stave "bass" ctx  
  notes <- Vx.createNotes voices
  voicing <- Vx.addNotesToVoice notes (Vx.createNewVoice 4.0 4.0)
  Vx.formatter voicing 500.0
  Vx.drawVoice ctx stave voicing
  
voices :: Vx.Bar
voices = [[{pitch: ["c/4", "f/4", "g/4"], duration: "h"}
          ,{pitch: ["b/5"], duration: "h"}
           ]
         , [{pitch: ["a/5"], duration: "w"}]
         , [{pitch: ["c/5"], duration: "h"}
           ,{pitch: ["d/5"], duration: "q"}
           ,{pitch: ["g/5"], duration: "q"}]
          ]
