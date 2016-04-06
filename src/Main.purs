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
import Data.Tuple
import Data.Maybe
--y
foreign import data CONSOLE :: !

type AccidentalBar = (Array (Array (Array (Tuple String Int))))

main :: forall e. Eff (dom :: DOM, midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW | e) Unit
main = do
  MidiPlayer.loadFile "bower_components/purescript-midiplayer/midi/ties4.mid"
  MidiPlayer.loadPlugin { soundfontUrl: "bower_components/midi/examples/soundfont/"
                  , instrument:   "acoustic_grand_piano"
                  }
    (const (renderNotation MidiPlayer.getData3))

renderNotation :: forall e. Eff (dom :: DOM, midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW | e) (Array Foreign) -> Eff (dom :: DOM, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI | e) Unit
renderNotation dat = do
  midiObjects <- dat
  ticksPerBeat <- MidiPlayer.getTicksPerBeat
  logger ticksPerBeat
  let sortedData :: Array MidiEvent
      sortedData = map unsafeF1 midiObjects
  let numerator :: Int
      numerator = getNumerator $ toList sortedData
  let bar :: Array MidiNote
      bar =  toUnfoldable $ Data.List.filter (\x -> x.noteNumber > 0) $ map (quantizeNote ticksPerBeat 0.0) $ calculateDuration $ midiEventWriter $ filterNotes $ toList sortedData
  logger $ map (\x -> x.deltaTime) bar
  logger bar
  -- let measuredMidi :: Array (Array MidiNote)
  let measuredMidi = toArray $ map (\x -> Data.List.concat <<< setTies ticksPerBeat 3.0 0.0 Nil $ x)$ divideIntoMeasures ticksPerBeat 3 0.0 Nil $ Data.List.filter (\x -> x.noteNumber > 0) $ toList bar
  logger measuredMidi
  let aap :: Array (Array (Int))
      aap = toArray $ map justToIndex $ map findStartingTie $ map (\x -> Data.List.concat <<< setTies ticksPerBeat 3.0 0.0 Nil $ x)$ divideIntoMeasures ticksPerBeat 3 0.0 Nil $ Data.List.filter (\x -> x.noteNumber > 0) $ toList bar
  logger aap
  let toVexFlow = map (\x -> [map (midiNoteToVexFlowNote ticksPerBeat) x]) measuredMidi
  logger toVexFlow
  canvas <- createCanvas "notationCanvas"
  renderer <- createRenderer canvas
  drawPrimaryStave renderer clef "C"
  logger $ testMusic toVexFlow
  drawNotation (testMusic toVexFlow) (musicWithIndexedAccidentals toVexFlow) renderer aap

toArray :: forall a. List (List a) -> Array (Array a)
toArray lst = toUnfoldable $ map (\x -> toUnfoldable x) lst

drawNotation :: forall e. VexFlowMusic -> Array AccidentalBar -> VexFlow -> Array (Array Int) -> Eff (dom :: DOM, vexFlow :: VEXFLOW | e) Unit
drawNotation music accidentals renderer indexedTies = do
  let stave = drawStave renderer 280.0 1.0
  let voices = Data.Array.zipWith drawVoice music accidentals
  let drawVoices = Data.Foldable.traverse_ (\(Tuple i voice) -> stave i $ voice $ fromJust $ index indexedTies i ) $ addIndexToArray voices
  drawVoices
  -- logger drawVoices

-- lastNoteHasFirstTie :: VexFlowBar -> Array Int -> Boolean
-- lastNoteHasFirstTie vxBar arr = length vxBar == fromJust (last arr)

drawVoice :: forall e. VexFlowBar -> AccidentalBar -> Array Int -> VexFlow -> VexFlow -> Eff (dom :: DOM, vexFlow :: VEXFLOW | e) Unit
drawVoice bar accidentals indexedTies context stave = do
  notes <- createNotes bar
  logger "hoi"
  logger notes
  addedAccidentals <- addAccidentals notes accidentals
  logger indexedTies
  tiedNotes <- addTies addedAccidentals indexedTies
  -- beamedNotes <- addBeams addedAccidentals
  voicing <- addNotesToVoice addedAccidentals (createNewVoice 3.0 4.0)
  formatter voicing (260.0)
  logger tiedNotes
  VexFlow.drawVoice context stave voicing
  drawTies tiedNotes context
  -- drawBeams beamedNotes context

justToIndex :: List (Maybe Int) -> List Int
justToIndex Nil = Nil
justToIndex (Cons x xs) | x == Nothing = (justToIndex xs)
justToIndex (Cons x xs) = Cons (fromJust x) (justToIndex xs)

fromJust :: forall a. Maybe a -> a
fromJust (Just x) = x

findStartingTie :: List MidiNote -> List (Maybe Int)
findStartingTie Nil = Nil
findStartingTie midiNotes@(Cons x xs) = mapWithIndex (\x n -> if x.hasFirstTie == true then Just n else Nothing) midiNotes

mapWithIndex :: forall a b. (a -> Int -> b) -> List a -> List b
mapWithIndex f lst = Data.List.reverse $ go 0 lst Nil
  where
  go _ Nil acc = acc
  go n (Cons x xs) acc = go (n+1) xs $ Cons (f x n) acc

drawStave :: forall e. VexFlow -> Number -> Number -> Int -> (VexFlow -> VexFlow -> (Eff (vexFlow :: VEXFLOW | e) Unit)) -> Eff (vexFlow :: VEXFLOW | e) Unit
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

addIndexToArray :: forall a. Array a  -> Array (Tuple Int a)
addIndexToArray arr = Data.List.Lazy.toUnfoldable $ Data.List.Lazy.zip (Data.List.Lazy.iterate (_ + 1) 0) (Data.List.Lazy.fromFoldable arr)


filterNotes :: List MidiEvent -> List MidiEvent
filterNotes Nil                      = Nil
filterNotes (Cons y@(NoteOn x) xs)   = Cons y (filterNotes xs)
filterNotes (Cons y@(NoteOff x) xs)  = Cons y (filterNotes xs)
filterNotes (Cons x xs)              = filterNotes xs

getNumerator :: List MidiEvent -> Int
getNumerator (Cons y@(TimeSignature x) xs) = x.numerator
getNumerator (Cons x xs)                   = getNumerator xs

getDeltaTime :: List MidiEvent -> List Number
getDeltaTime Nil                     = Nil
getDeltaTime (Cons y@(NoteOn x) xs)  = Cons x.deltaTime (getDeltaTime xs)
getDeltaTime (Cons y@(NoteOff x) xs) = Cons x.deltaTime (getDeltaTime xs)
getDeltaTime (Cons x xs)             = getDeltaTime xs

separateStaff :: List MidiEvent -> List MidiEvent
separateStaff Nil                      = Nil
separateStaff (Cons y@(NoteOn x) xs)  | x.noteNumber > 61 = Cons y (separateStaff xs)
separateStaff (Cons y@(NoteOff x) xs) | x.noteNumber > 61 = Cons y (separateStaff xs)
separateStaff (Cons x xs)                                 = separateStaff xs
