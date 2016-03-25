module Main where

import Prelude (Unit, (+), (-), ($), bind, (*), const, show, map, (==), mod, (||), (>), (/), (<))
import Control.Monad.Eff (Eff)
import Data.Tuple (Tuple(Tuple))
import VexFlow
import VexMusic
import Music (KeySignature, Clef)
import MidiJS as MidiPlayer
import MidiJsTypes
import Data.Maybe
import Data.Array (filter)
import Data.Foreign
import Data.Either
import Data.Foldable (foldl)
import Data.List

type AccidentalBar = (Array (Array (Array (Tuple String Int))))

main :: forall e. Eff (midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW, dom :: DOM | e) Unit
main = do
  MidiPlayer.loadFile "bower_components/purescript-midiplayer/midi/tiedBar.mid"
  MidiPlayer.loadPlugin { soundfontUrl: "bower_components/midi/examples/soundfont/"
                  , instrument:   "acoustic_grand_piano"
                  }
    (const (renderNotation MidiPlayer.getData))

renderNotation :: forall e. (Eff (dom :: DOM, vexFlow :: VEXFLOW | e) (Array Foreign)) -> Eff (vexFlow :: VEXFLOW, dom :: DOM | e) Unit
renderNotation dat = do
  canvas <- createCanvas "notationCanvas"
  renderer <- createRenderer canvas
  drawPrimaryStave renderer clef "B"
  drawNotation (testMusic eighthsMusic) (musicWithIndexedAccidentals eighthsMusic) renderer
  midiObjects <- dat
  let noteOnOff :: Array Foreign
      noteOnOff = filter (hasSubType "noteOn" || hasSubType "noteOff" ) midiObjects
  logger $ toArray $ divideIntoMeasures 0 Nil (toList noteOnOff)

toArray :: forall a. List (List a) -> Array (Array a)
toArray xs = toUnfoldable $ map toUnfoldable xs

lastNoteDuration :: forall e. Array Foreign -> Eff (dom :: DOM, midi :: MidiPlayer.MIDI | e) Unit
lastNoteDuration midiTrack = do
  let songDuration :: Int
      songDuration = foldl (\acc midiEvent -> (showDeltaTime midiEvent) + acc) 0 midiTrack
  ticksPerBeat <- MidiPlayer.getTicksPerBeat
  logger ticksPerBeat
  logger $ typeOf $ toForeign ticksPerBeat
  let trackDuration = ticksPerBeat + songDuration
  logger trackDuration


hasSubType :: String -> Foreign -> Boolean
hasSubType subType midiEvent = showSubType midiEvent == subType

showNote :: Foreign -> { pitch :: Array VexFlowPitch
                       , duration :: Int
                       , subType  :: String}            
showNote midiEvent = { pitch    : [show $ showPitchNumber midiEvent]
                     , duration : showDeltaTime midiEvent
                     , subType  : showSubType midiEvent}
                     

showDeltaTime :: Foreign -> Int
showDeltaTime midiEvent = (toMidiEvent $ fromRight $ Data.Foreign.Index.index 0 midiEvent).event.deltaTime

showPitchNumber :: Foreign -> Int
showPitchNumber midiEvent = (toMidiEvent $ fromRight $ Data.Foreign.Index.index 0 midiEvent).event.noteNumber

showSubType :: Foreign -> String
showSubType midiEvent = (toMidiEvent $ fromRight $ Data.Foreign.Index.index 0 midiEvent).event.subtype

toMidiEvent :: Foreign -> MidiEvent2
toMidiEvent = unsafeFromForeign

fromRight :: forall a b. Either a b -> b
fromRight (Right a) = a

drawNotation :: forall e. VexFlowMusic -> Array AccidentalBar -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
drawNotation music accidentals renderer = do
  let stave = drawStave renderer 280.0 1.0
  let voices = Data.Array.zipWith drawVoice music accidentals
  Data.Foldable.traverse_ (\(Tuple i voice) -> stave i voice) $ addIndexToArray voices

drawVoice :: forall e. VexFlowBar -> AccidentalBar -> VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
drawVoice bar accidentals context stave = do
  notes <- createNotes bar
  addedAccidentals <- addAccidentals notes accidentals
  beamedNotes <- addBeams addedAccidentals
  voicing <- addNotesToVoice addedAccidentals (createNewVoice 4.0 4.0)
  formatter voicing (260.0)
  VexFlow.drawVoice context stave voicing
  drawBeams beamedNotes context

drawStave :: forall e. VexFlow -> Number -> Number -> Int -> (VexFlow -> VexFlow -> (Eff (vexFlow :: VEXFLOW | e) Unit)) -> Eff (vexFlow :: VEXFLOW| e) Unit
drawStave renderer w y x voice = do
  ctx <- createCtx renderer
  stave <- createStave (80 + x * 280) y w
  VexFlow.drawStave stave ctx
  voice ctx stave

drawPrimaryStave :: forall e. VexFlow -> Clef -> KeySignature -> Eff (vexFlow :: VEXFLOW | e) Unit
drawPrimaryStave renderer clef key = do
    ctx <- createCtx renderer
    stave <- createStave 1 1.0 80.0
    createKeySignature key stave
    createTimeSignature "4/4" stave
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

addIndexToArray :: forall a. Array a  -> Array (Tuple Int a)
addIndexToArray arr = Data.List.Lazy.toUnfoldable $ Data.List.Lazy.zip (Data.List.Lazy.iterate (_ + 1) 0) (Data.List.Lazy.fromFoldable arr)

getPPQ = 480
measureLength = getPPQ * 4
type VexFoo = {pitch :: Array VexFlowPitch, duration :: Int, subType :: String}

measure1 = 1920
               
divideIntoMeasures :: Int -> List VexFoo -> List Foreign -> List (List VexFoo)
divideIntoMeasures accumulatedDeltaTime accXs Nil = Nil
divideIntoMeasures accumulatedDeltaTime accXs (Cons x xs) = if accumulatedDeltaTime + (showDeltaTime x) < measure1 then
                               divideIntoMeasures ((showDeltaTime x) + accumulatedDeltaTime) (snoc accXs (showNote x)) xs
                             else if accumulatedDeltaTime + (showDeltaTime x) == measure1 then
                                    (Cons (snoc accXs (showNote x)) (divideIntoMeasures 0 Nil xs))
                                  else
                                    (Cons (snoc accXs (insertNewDeltaTime x x1))) (divideIntoMeasures x2 (toList [insertNewDeltaTime x x2]) xs)
                                    where
                                      x1 = measure1 - accumulatedDeltaTime
                                      x2 = (showDeltaTime x) - x1

insertNewDeltaTime midiEvent n =  { pitch    : [show $ showPitchNumber midiEvent]
                                  , duration : n
                                  , subType  : showSubType midiEvent}
