module Main where

import Prelude
-- import Control.Apply
-- import Control.Monad.Eff
-- import Control.Monad.Eff.Console

import Data.Tuple
import VexFlow as Vx
import VexMusic as Vm
import Music

type AccidentalBar = (Array (Array (Array (Tuple String Int))))

main :: Vx.VexFlowEff
main = do
  canvas <- Vx.createCanvas "notationCanvas"
  renderer <- Vx.createRenderer canvas
  drawNotation (Vm.aapMuzak Vm.eighthsMusic) (Vm.aapTuple Vm.eighthsMusic) renderer
  -- Vx.logger $ Vm.aapTuple Vm.eighthsMusic


drawNotation :: Vm.VexFlowMusic -> Array AccidentalBar -> Vx.VexFlow -> Vx.VexFlowEff
drawNotation music accidentals renderer = do
  let stave = drawStave renderer 1.0 1.0 280.0
  let voices = (Data.Array.zipWith drawVoice music accidentals)
  Data.Foldable.traverse_ stave voices
  Vx.logger voices 

drawVoice :: Vm.VexFlowBar -> AccidentalBar -> Vx.VexFlow -> Vx.VexFlow -> Vx.VexFlowEff
drawVoice bar accidentals context stave = do
  notes <- Vx.createNotes bar
  addedAccidentals <- Vx.addAccidentals notes accidentals
  beamedNotes <- Vx.addBeams addedAccidentals
  voicing <- Vx.addNotesToVoice addedAccidentals (Vx.createNewVoice 4.0 4.0)
  Vx.formatter voicing (260.0)
  Vx.drawVoice context stave voicing
  Vx.drawBeams beamedNotes context

drawStave :: Vx.VexFlow -> Number -> Number -> Number -> (Vx.VexFlow -> Vx.VexFlow -> Vx.VexFlowEff) -> Vx.VexFlowEff
drawStave renderer x y w voice = do
  ctx <- Vx.createCtx renderer
  stave <- Vx.createStave x y w
  Vx.drawStave stave ctx
  voice ctx stave

drawPrimaryStave :: Vx.VexFlow -> Clef -> KeySignature -> Vx.VexFlowEff
drawPrimaryStave renderer clef key = do
    ctx <- Vx.createCtx renderer
    stave <- Vx.createStave 1.0 1.0 80.0
    Vx.createKeySignature key stave
    Vx.createTimeSignature "4/4" stave
    Vx.drawKeyStave stave clef ctx

drawTrebleStave :: Number -> Vx.VexFlow -> KeySignature -> Vx.VexFlowEff
drawTrebleStave y renderer key = do
    ctx <- Vx.createCtx renderer
    stave <- Vx.createStave 1.0 y 80.0
    Vx.createKeySignature key stave
    Vx.createTimeSignature "4/4" stave
    Vx.drawKeyStave stave "treble" ctx

drawBassStave :: Number -> Vx.VexFlow -> KeySignature -> Vx.VexFlowEff
drawBassStave y renderer key = do
    ctx <- Vx.createCtx renderer
    stave <- Vx.createStave 1.0 y 80.0
    Vx.createKeySignature key stave
    Vx.createTimeSignature "4/4" stave
    Vx.drawKeyStave stave "bass" ctx

-- drawTrebleStaveLine :: Number -> Vm.VexFlowVoice -> Vx.VexFlow -> Vx.VexFlowEff    
-- drawTrebleStaveLine y notes renderer = do
--   drawTrebleStave y renderer "G"
--   let trebleStave1 = drawStave renderer 80.0 y 280.0
--   trebleStave1 drawVoice 
--   let trebleStave2 = drawStave renderer 360.0 y 280.0
--   trebleStave2 drawVoice
--   let trebleStave3 = drawStave renderer 640.0 y 280.0
--   trebleStave3 drawVoice
--   let trebleStave4 = drawStave renderer 920.0 y 280.0
--   trebleStave4 drawVoice

-- drawBassStaveLine :: Number -> Vm.VexFlowVoice -> Vx.VexFlow -> Vx.VexFlowEff    
-- drawBassStaveLine y notes renderer = do
--   drawBassStave y renderer "G"
--   let bassStave1 = drawStave renderer 80.0 y 280.0
--   bassStave1 drawVoice
--   let bassStave2 = drawStave renderer 360.0 y 280.0
--   bassStave2 drawVoice
--   let bassStave3 = drawStave renderer 640.0 y 280.0
--   bassStave3 drawVoice
--   let bassStave4 = drawStave renderer 920.0 y 280.0
--   bassStave4 drawVoice

-- drawKeyedStave :: Vx.VexFlow -> Number -> Number -> Number -> KeySignature -> (Vx.VexFlow -> Vx.VexFlow -> Vx.VexFlowEff) -> Vx.VexFlowEff
-- drawKeyedStave renderer x y w key voice = do
--   ctx <- Vx.createCtx renderer
--   stave <- Vx.createStave x y w
--   Vx.createKeySignature key stave
--   Vx.drawStave stave ctx
--   voice ctx stave

  
    
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

vexAccidentalBar2 :: Array (Array (Array (Tuple String Int)))
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
