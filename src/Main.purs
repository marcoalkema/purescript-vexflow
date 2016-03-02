module Main where

import Prelude
-- import Control.Monad.Eff
-- import Control.Monad.Eff.Console
import Data.Tuple
import VexFlow as Vx
import VexMusic as Vm
import Music

main :: Vx.VexFlowEff
main = do
  canvas <- Vx.createCanvas "notationCanvas"
  renderer <- Vx.createRenderer canvas
  drawKeySignatureStave renderer "treble" "G"
  let stave = drawStave renderer 80.0 1.0 280.0
  stave drawVoice
  let stave2 = drawStave renderer 360.0 1.0 280.0
  stave2 drawVoice
  -- Vx.logger notes

drawKeySignatureStave :: Vx.VexFlow -> Clef -> KeySignature -> Vx.VexFlowEff
drawKeySignatureStave renderer clef key = do
    ctx <- Vx.createCtx renderer
    stave <- Vx.createStave 1.0 1.0 80.0
    Vx.createKeySignature key stave
    Vx.drawKeyStave stave clef ctx

drawStave :: Vx.VexFlow -> Number -> Number -> Number -> (Vx.VexFlow -> Vx.VexFlow -> Vx.VexFlowEff) -> Vx.VexFlowEff
drawStave renderer x y w voice = do
  ctx <- Vx.createCtx renderer
  stave <- Vx.createStave x y w
  Vx.drawStave stave ctx
  voice ctx stave

drawVoice :: Vx.VexFlow -> Vx.VexFlow -> Vx.VexFlowEff
drawVoice context stave = do
  notes <- Vx.createNotes vexBar2
  addedAccidentals <- Vx.addAccidentals notes vexAccidentalBar2
  beamedNotes <- Vx.addBeams addedAccidentals
  voicing <- Vx.addNotesToVoice addedAccidentals (Vx.createNewVoice 4.0 4.0)
  Vx.formatter voicing (260.0)
  Vx.drawVoice context stave voicing
  Vx.drawBeams beamedNotes context
    
vexBar :: Vm.VexFlowBar
vexBar = [[{pitch: ["c/4", "f/4", "g/4"], duration: "h"}, {pitch: ["b/5"], duration: "h"}]           
         ,[{pitch: ["ab/5"], duration: "1"}]          
         ,[{pitch: ["c/5"], duration: "2"}
           ,{pitch: ["d/5"], duration: "8"}
           ,{pitch: ["g/5"], duration: "8"}
           ,{pitch: ["d/5"], duration: "8"}
           ,{pitch: ["g/5"], duration: "8"}
           ]
          ]

vexBar2 :: Vm.VexFlowBar
vexBar2 =  [[{pitch: ["d/4"], duration: "8"}
            ,{pitch: ["g/4"], duration: "8"}
            ,{pitch: ["d/4"], duration: "8"}
            ,{pitch: ["g/4"], duration: "8"}
            ,{pitch: ["g/4"], duration: "8"}
            ,{pitch: ["d/4"], duration: "8"}
            ,{pitch: ["g/4"], duration: "8"}
            ,{pitch: ["g/4"], duration: "8"}]]

vexAccidentalBar2 :: Array (Array (Array (Tuple Int String)))
vexAccidentalBar2 = [[[],[],[],[],[],[],[],[]]]

vexAccidentalBar :: Array (Array (Array (Tuple Int String)))
vexAccidentalBar = [ [[(Tuple 0 "#"), (Tuple 1 "b")], [(Tuple 0 "#")]]
                   , [[Tuple 0 "b"]]
                   , [[(Tuple 0 "#")], [], [], [(Tuple 0 "b")], [(Tuple 0 "b")]]
                   ]
                      
testVoice :: Vm.VexFlowVoice
testVoice = [{pitch: ["c#/5"], duration: "2"}
            ,{pitch: ["d##/5"], duration: "8"}
            ,{pitch: ["gbb/5"], duration: "8"}
            ,{pitch: ["d##/5"], duration: "8"}
            ,{pitch: ["gbb/5"], duration: "8"}
            ]

voicesCis :: Vm.VexFlowBar
voicesCis = [[]]
