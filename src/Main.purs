module Main where

import Prelude
-- import Control.Monad.Eff
-- import Control.Monad.Eff.Console
import Data.Tuple
import VexFlow as Vx
import VexMusic as Vm
import Music

type AccidentalBar = (Array (Array (Array (Tuple String Int))))

main :: Vx.DomEff
main = do
  canvas <- Vx.createCanvas "notationCanvas"
  renderer <- Vx.createRenderer canvas
  drawNotation (Vm.testMusic Vm.eighthsMusic) (Vm.musicWithIndexedAccidentals Vm.eighthsMusic) renderer

drawNotation :: Vm.VexFlowMusic -> Array AccidentalBar -> Vx.VexFlow -> Vx.DomEff
drawNotation music accidentals renderer = do
  let stave = drawStave renderer 1.0 1.0 280.0
  let voices = Data.Array.zipWith drawVoice music accidentals
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
