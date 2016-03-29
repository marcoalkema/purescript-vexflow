module Main where

-- import Prelude (Unit, (+), (-), ($), bind, (*), const, show, map, (==), (||), (<), (&&))
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console
import Data.Tuple (Tuple(Tuple), fst, snd)
import VexFlow
import VexMusic
import Music (KeySignature, Clef)
import MidiJS as MidiPlayer
import MidiJsTypes
import Data.Foreign (Foreign, unsafeFromForeign, toForeign, typeOf)
import Data.Foldable (foldl)
import Data.List (List(Nil, Cons), snoc, toUnfoldable, toList, delete)
import Data.Either
import MidiToVexFlow
import Data.Array

foreign import data CONSOLE :: !

type AccidentalBar = (Array (Array (Array (Tuple String Int))))

main :: forall e. Eff (midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW, dom :: DOM | e) Unit
main = do
  MidiPlayer.loadFile "bower_components/purescript-midiplayer/midi/1bar8s3.mid"
  MidiPlayer.loadPlugin { soundfontUrl: "bower_components/midi/examples/soundfont/"
                  , instrument:   "acoustic_grand_piano"
                  }
    (const (renderNotation MidiPlayer.getData3))

renderNotation :: forall e. (Eff (dom :: DOM, vexFlow :: VEXFLOW | e) (Array Foreign)) -> Eff (vexFlow :: VEXFLOW, dom :: DOM | e) Unit
renderNotation dat = do
  midiObjects <- dat
  let sortedData = map unsafeF1 midiObjects
  let measuredMidi :: Array (Array MidiNote)
      measuredMidi = toArray $ divideIntoMeasures 0 Nil $ calculateDuration $ midiEventWriter $ filterData $ toList sortedData
  let toVexFlow = map (\x -> [map midiNoteToVexFlowNote x]) measuredMidi
  canvas <- createCanvas "notationCanvas"
  renderer <- createRenderer canvas
  drawPrimaryStave renderer clef "C"
  drawNotation (testMusic toVexFlow) (musicWithIndexedAccidentals toVexFlow) renderer

  -- let processed :: Array { noteNumber    :: Int
  --                        , deltaTime :: Int}
  --     processed = toUnfoldable $ calculateDuration $ midiEventWriter $ toList sortedData
  -- let noteOff :: Tuple MidiEventFoo Boolean
  --     noteOff =  fromRight $ findNoteOff 74 $ midiEventWriter $ toList sortedData

  -- let processed1 :: Array (Array { noteNumber    :: Int
  --                                , deltaTime :: Int})
  --     processed1 = toArray $ divideIntoMeasures 0 Nil $ calculateDuration $ midiEventWriter $ toList sortedData
  -- logger processed1

filterData :: List MidiEventFoo -> List MidiEventFoo
filterData Nil = Nil
filterData (Cons y@(NoteOn x) xs) = (Cons y (filterData xs))
filterData (Cons y@(NoteOff x) xs ) = (Cons y (filterData xs))
filterData (Cons x xs) = filterData xs
  
toArray :: forall a. List (List a) -> Array (Array a)
toArray lst = toUnfoldable $ map (\x -> toUnfoldable x) lst

drawNotation :: forall e. VexFlowMusic -> Array AccidentalBar -> VexFlow -> Eff (vexFlow :: VEXFLOW, dom :: DOM | e) Unit
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

-- drawStave :: forall e. VexFlow -> Number -> Number -> Int -> (VexFlow -> VexFlow -> (Eff (vexFlow :: VEXFLOW | e) Unit)) -> Eff (vexFlow :: VEXFLOW, dom :: DOM | e) Unit
drawStave renderer w y x voice = do
  ctx <- createCtx renderer
  stave <- createStave (80 + x * 280) y w
  VexFlow.drawStave stave ctx
  voice ctx stave
  logger $ show x

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
