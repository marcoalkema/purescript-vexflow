module VexFlow where

import Prelude
import Control.Monad.Eff (Eff)
import Music (TimeSignature, KeySignature, Clef)
import VexMusic
import Data.Tuple (Tuple)
import Data.Maybe
import Data.Tuple
import Data.List

type AccidentalBar = (Array (Array (Array (Tuple String Int))))

foreign import data VEXFLOW       :: !
foreign import data VexFlow       :: *
foreign import data CANVAS        :: !
foreign import data Canvas        :: *                    
foreign import data DOM           :: !
foreign import data Dom           :: *                    

foreign import clef               :: String
foreign import createCanvas       :: forall e. String -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createRenderer     :: forall e. VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createCtx          :: forall e. VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createStave        :: forall e. Int -> Number -> Number -> Eff (vexFlow :: VEXFLOW | e) VexFlow
-- Eff in DOM
foreign import drawStave          :: forall e. VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawKeyStave       :: forall e. VexFlow -> Clef -> VexFlow  -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import createKeySignature :: forall e. KeySignature -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createTimeSignature :: forall e. TimeSignature -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createNotes        :: forall e. VexFlowBar -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addAccidentals     :: forall e. VexFlow -> Array (Array (Array (Tuple String Int))) -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addTies            :: forall e. VexFlow -> Array Int  -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addBeams           :: forall e. VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createNewVoice     :: forall e. Number  -> Number -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addNotesToVoice    :: forall e. VexFlow -> (Eff (vexFlow :: VEXFLOW) VexFlow) -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import formatter          :: forall e. VexFlow -> Number -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawVoice          :: forall e. VexFlow -> VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawBeams          :: forall e. VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawTies           :: forall e. VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit

foreign import logger             :: forall a. a -> forall e. Eff (dom :: DOM | e) Unit

renderNotation :: forall e. VexMusic -> Array (Array Int) -> Eff (dom :: DOM, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI | e) Unit
renderNotation dat indexedTies = do
  canvas <- createCanvas "notationCanvas"
  renderer <- createRenderer canvas
  drawPrimaryStave renderer clef "G"
  drawNotation (testMusic dat) (musicWithIndexedAccidentals dat) renderer indexedTies

drawNotation :: forall e. VexFlowMusic -> Array AccidentalBar -> VexFlow -> Array (Array Int) -> Eff (vexFlow :: VEXFLOW | e) Unit
drawNotation music accidentals renderer indexedTies = do
  let stave = renderStaff renderer 280.0 1.0
  let voices = Data.Array.zipWith renderVoice music accidentals
  let drawVoices = Data.Foldable.traverse_ (\(Tuple i voice) -> stave i $ voice $ fromJust $ Data.Array.index indexedTies i ) $ addIndexToArray voices
  drawVoices

-- lastNoteHasFirstTie :: VexFlowBar -> Array Int -> Boolean
-- lastNoteHasFirstTie vxBar arr = length vxBar == fromJust (last arr)

renderVoice :: forall e. VexFlowBar -> AccidentalBar -> Array Int -> VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
renderVoice bar accidentals indexedTies context stave = do
  notes <- createNotes bar
  addedAccidentals <- addAccidentals notes accidentals
  tiedNotes <- addTies addedAccidentals indexedTies
  -- beamedNotes <- addBeams addedAccidentals
  voicing <- addNotesToVoice addedAccidentals (createNewVoice 3.0 4.0)
  formatter voicing (260.0)
  drawVoice context stave voicing
  drawTies tiedNotes context
  -- drawBeams beamedNotes context

renderStaff :: forall e. VexFlow -> Number -> Number -> Int -> (VexFlow -> VexFlow -> (Eff (vexFlow :: VEXFLOW | e) Unit)) -> Eff (vexFlow :: VEXFLOW | e) Unit
renderStaff renderer w y x voice = do
  ctx <- createCtx renderer
  stave <- createStave (80 + x * 280) y w
  drawStave stave ctx
  voice ctx stave

drawPrimaryStave :: forall e. VexFlow -> Clef -> KeySignature -> Eff (vexFlow :: VEXFLOW | e) Unit
drawPrimaryStave renderer clef key = do
    ctx <- createCtx renderer
    stave <- createStave 1 1.0 80.0
    createKeySignature key stave
    createTimeSignature "3/4" stave
    drawKeyStave stave clef ctx

-- drawTrebleStave :: Number -> Vx.VexFlow -> KeySignature -> Vx.VexFlowEff
-- drawTrebleStave y renderer key = do
--     ctx <- Vx.createCtx renderer
--     stave <- Vx.createStave 1.0 1 80.0
--     Vx.createKeySignature key stave
--     Vx.createTimeSignature "4/4" stave
--     Vx.drawKeyStave stave "treble" ctx

-- drawBassStave :: Number -> Vx.VexFlow -> KeySignature -> Vx.VexFlowEff
-- drawBassStave y renderer key = do
--     ctx <- Vx.createCtx renderer
--     stave <- Vx.createStave 1.0 y 80.0
--     Vx.createKeySignature key stave
--     Vx.createTimeSignature "4/4" stave
--     Vx.drawKeyStave stave "bass" ctx


fromJust :: forall a. Maybe a -> a
fromJust (Just x) = x

mapWithIndex :: forall a b. (a -> Int -> b) -> List a -> List b
mapWithIndex f lst = Data.List.reverse $ go 0 lst Nil
  where
  go _ Nil acc = acc
  go n (Cons x xs) acc = go (n+1) xs $ Cons (f x n) acc

addIndexToArray :: forall a. Array a  -> Array (Tuple Int a)
addIndexToArray arr = Data.List.Lazy.toUnfoldable $ Data.List.Lazy.zip (Data.List.Lazy.iterate (_ + 1) 0) (Data.List.Lazy.fromFoldable arr)
