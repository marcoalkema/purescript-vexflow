module Main where

import Prelude
-- import Control.Monad.Eff
-- import Control.Monad.Eff.Console
import VexFlow as Vx
import VexMusic as Vm

main :: Vx.VexFlowEff
main = do
  canvas <- Vx.createCanvas "notationCanvas"
  renderer <- Vx.createRenderer canvas
  ctx <- Vx.createCtx renderer
  stave <- Vx.createStave 1.0 1.0 500.0
  Vx.createKeySignature "D" stave
  Vx.drawStave stave "treble" ctx
  notes <- Vx.createNotes voicesCis 
  voicing <- Vx.addNotesToVoice notes (Vx.createNewVoice 4.0 4.0)  
  Vx.formatter voicing 500.0
  Vx.drawVoice ctx stave voicing
  
voices :: Vm.VexFlowBar
voices = [[{pitch: ["c/4", "f/4", "g/4"], duration: "h"}
          ,{pitch: ["b/5"], duration: "h"}
           ]
         , [{pitch: ["ab/5"], duration: "1"}]
         , [{pitch: ["c#/5"], duration: "2"}
           ,{pitch: ["d##/5"], duration: "8"}
           ,{pitch: ["gbb/5"], duration: "8"}
           ,{pitch: ["d##/5"], duration: "8"}
           ,{pitch: ["gbb/5"], duration: "8"}
           ]
          ]

voicesCis :: Vm.VexFlowBar
voicesCis = [[]]
