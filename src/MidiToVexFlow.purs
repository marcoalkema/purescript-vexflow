module MidiToVexFlow where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(Nil, Cons), snoc)
import Data.Either (Either(Right, Left))
import Data.Foreign
import Data.Tuple
import MidiJsTypes
import MidiPlayer
import MidiJS as MidiPlayer2
import VexMusic
import Data.Foldable (foldl)
import Data.Maybe
import VexFlow
import Data.Int
import Data.List
import Data.List (concat, filter)
import Quantizer

type TicksPerBeat = Number
type Numerator = Int
type DeltaTime = Number

foreign import unsafeF1 :: Foreign -> MidiEvent

type VexFlowResult = {
  vexFlowNotes :: VexFlowMusic
, vexNotes     :: VexMusic
, indexedTies  :: Array (Array TieIndex)
, indexedBeams :: Array (Array (Array BeamIndex))
, numerator    :: Int
}
--kip
renderMidiPuurJwt :: Array Foreign -> Number -> VexFlowResult
renderMidiPuurJwt midiData ticksPerBeat = do
  let safeData  = toList $ map unsafeF1 midiData
      numerator = getNumerator safeData
      midiNotes = filter (\x -> x.noteNumber > 0)
                  <<< map (quantizeNote ticksPerBeat 0.0)
                  <<< calculateDuration
                  <<< map (\midiObject -> Tuple midiObject false) -- midiEventWriter
                  <<< filter metsj
                  $ toList safeData
      measuredMidi = map ((concat <<< setTies ticksPerBeat numerator 0.0 Nil) <<<
                          (concat <<< setDots ticksPerBeat numerator 0.0 Nil))
                     $ divideIntoMeasures ticksPerBeat numerator 0.0 Nil midiNotes
  let foo :: Array (Array (Tuple Number MidiNote))
      foo = toArray $ map (position' ticksPerBeat) measuredMidi
  let indexedTies = toArray $ map (maybeToInt <<< findStartingTie) measuredMidi
      indexedBeams :: Array (Array (Array BeamIndex))
      indexedBeams = toUnfoldable $ map ((toArray <<< beamsIndex ticksPerBeat Nil) <<< (eighthsIndex' ticksPerBeat)) measuredMidi
      vexFlowNotes = map (\x -> [map (midiNoteToVexFlowNote ticksPerBeat) x]) $ toArray measuredMidi
      vexNotes     = map (\x -> [map (midiNoteToVexNote ticksPerBeat) x]) $ toArray measuredMidi
  { vexFlowNotes, vexNotes, indexedTies, indexedBeams, numerator}

-- TODO glue, eliminate
-- renderMidi :: forall e. Canvas -> Array Foreign -> Eff (vexFlow :: VEXFLOW, midi :: MIDI | e) Unit
renderMidi canvas d = do
  ticksPerBeat <- getTicksPerBeat
  let x = renderMidiPuurJwt d ticksPerBeat
  renderNotation canvas x.vexFlowNotes x.vexNotes x.indexedTies x.indexedBeams x.numerator

-- TODO expliciete recursie wegwerken (fold ofzo)
--FIX ALL FOR NOTEOFFS/RESTS
-- foo :: TicksPerBeat -> Numerator -> DeltaTime -> List MidiNote -> List (List MidiNote) -> List (List MidiNote)
-- foo tpb num accDt accNotes lst = foldl

divideIntoMeasures :: TicksPerBeat -> Numerator -> DeltaTime -> List MidiNote -> List MidiNote -> List (List MidiNote)
divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accNotes Nil = Nil
divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accNotes (Cons note notes) =
  if dt < measure then
         divideIntoMeasures ticksPerBeat numerator dt (snoc accNotes note) notes
  else if dt == measure then
         (Cons (snoc accNotes note) (divideIntoMeasures ticksPerBeat numerator 0.0 Nil notes))
  else
         (Cons (snoc accNotes (createStartTie note)))
         (divideIntoMeasures ticksPerBeat numerator 0.0 Nil (Cons (createEndingTie note) notes))
  where
    measure               = ticksPerBeat * (toNumber numerator)
    lastNoteDeltaTime     = measure - accumulatedDeltaTime
    newFirstNoteDeltaTime = note.deltaTime - lastNoteDeltaTime
    dt                    = accumulatedDeltaTime + note.deltaTime
    createStartTie        = setFirstTie <<< insertNewDeltaTime lastNoteDeltaTime
    createEndingTie       = setEndingTie <<< insertNewDeltaTime newFirstNoteDeltaTime 

-- duplicatie elimineren mbv if-then ipv guards

setTies :: TicksPerBeat -> Numerator -> DeltaTime -> List MidiNote -> List MidiNote -> List (List MidiNote)
setTies ticksPerBeat numerator accumulatedDeltaTime accNotes Nil = Cons accNotes Nil
setTies ticksPerBeat numerator accumulatedDeltaTime accNotes (Cons note notes) =
  if dt < ticksPerBeat then
         setTies ticksPerBeat numerator dt (snoc accNotes note) notes
  else if dt == ticksPerBeat then
         Cons (snoc accNotes note) (setTies ticksPerBeat numerator 0.0 Nil notes)
  else if dt == 2.0 * ticksPerBeat && (accumulatedDeltaTime == 0.0 || accumulatedDeltaTime == 960.0 ) then
         Cons (snoc accNotes note) (setTies ticksPerBeat numerator (mod ((ticksPerBeat * 4.0) - accumulatedDeltaTime) (ticksPerBeat * 4.0))  Nil notes)
  else if dt == 4.0 * ticksPerBeat then
         Cons (snoc accNotes note) (setTies ticksPerBeat numerator 0.0 Nil notes)
           else if dt > ticksPerBeat && note.hasDot then
         Cons (snoc accNotes note) (setTies ticksPerBeat numerator (if accumulatedDeltaTime == 0.0 then resolution else 0.0) Nil notes)
  else
         Cons (snoc accNotes (setFirstTie $ insertNewDeltaTime lastNoteDeltaTime note)) (setTies ticksPerBeat numerator 0.0 Nil (Cons (setEndingTie $ insertNewDeltaTime newFirstNoteDeltaTime note) notes))
  where
    lastNoteDeltaTime     = ticksPerBeat - accumulatedDeltaTime
    newFirstNoteDeltaTime = note.deltaTime - lastNoteDeltaTime
    dt                    = accumulatedDeltaTime + note.deltaTime
    resolution            = ticksPerBeat / 4.0

insertNewDeltaTime :: Number -> MidiNote -> MidiNote
insertNewDeltaTime n midiNote = midiNote { noteNumber   = midiNote.noteNumber
                                         , deltaTime    = n }

-- TODO underscrores jwt
setDots :: TicksPerBeat -> Numerator -> DeltaTime -> List MidiNote -> List MidiNote -> List (List MidiNote)
setDots _            _         _                    accXs Nil = Cons accXs Nil
setDots ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) | (accumulatedDeltaTime == 0.0 && x.deltaTime == ticksPerBeat * 1.5 )                       ||
                                                                        (accumulatedDeltaTime == ticksPerBeat / 2.0 && x.deltaTime == ticksPerBeat * 1.5)         ||
                                                                        (accumulatedDeltaTime == (ticksPerBeat / 2.0) * 4.0 && x.deltaTime == ticksPerBeat * 1.5) ||
                                                                        (accumulatedDeltaTime == (ticksPerBeat / 2.0) * 5.0 && x.deltaTime == ticksPerBeat * 1.5) = setDots ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs $ setDot x) xs
setDots ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) = setDots ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs x) xs

eighthsIndex' :: TicksPerBeat -> List MidiNote -> List (Tuple Int Number)
eighthsIndex' ticksPerBeat = map (\(Tuple x (Tuple y z)) -> Tuple x y) <<< filter (\(Tuple _ (Tuple _ note)) -> note.deltaTime <= (ticksPerBeat / 2.0) * 1.5) <<< indexed
  where
    indexed :: List MidiNote -> List (Tuple Int (Tuple Number MidiNote))
    indexed notes = Data.List.Lazy.toUnfoldable <<< Data.List.Lazy.zip (Data.List.Lazy.iterate (_ + 1) 0) <<< Data.List.Lazy.fromFoldable $ position' ticksPerBeat notes

position' :: Number -> List MidiNote -> List (Tuple Number MidiNote)
position' ticksPerBeat (Cons x xs) = Cons (Tuple 0.0 x) (position ticksPerBeat 0.0 (Cons x xs))

position :: Number -> Number -> List MidiNote -> List (Tuple Number MidiNote)
position ticksPerBeat _   Nil                   = Nil
position ticksPerBeat _   (Cons x Nil)          = Nil
position ticksPerBeat pos (Cons x (Cons y Nil)) = Cons (Tuple (pos + x.deltaTime) y)  Nil
position ticksPerBeat pos (Cons x (Cons y ys))  = Cons (Tuple (toNumber (mod (round (x.deltaTime + pos)) (round (ticksPerBeat * 4.0)))) y) (position ticksPerBeat (pos + x.deltaTime) (Cons y ys))

--foldr
beamsIndex :: Number -> List Int -> List (Tuple Int Number) -> List (List Int)
beamsIndex _ accXs Nil                               = Cons accXs          Nil
beamsIndex ticksPerBeat accXs (Cons x Nil)           = Cons (snoc accXs (fst x)) Nil
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | (snd x) == ((ticksPerBeat / 4.0) * 3.0) = Cons (snoc accXs (fst x)) (beamsIndex ticksPerBeat  Nil (Cons y ys))
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | (snd x) == ((ticksPerBeat / 4.0) * 11.0) = Cons (snoc accXs (fst x)) (beamsIndex ticksPerBeat  Nil (Cons y ys))
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | (snd x) == ((ticksPerBeat / 2.0) * 3.0) || (snd x) == ((ticksPerBeat / 4.0) * 7.0) = Cons (snoc accXs (fst x)) (beamsIndex ticksPerBeat  Nil (Cons y ys))
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | (fst x) /= (fst y) - 1 = Cons accXs (beamsIndex ticksPerBeat  Nil (Cons y ys))
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | (fst x) == (fst y) - 1 = beamsIndex ticksPerBeat (snoc accXs (fst x)) (Cons y ys)

-- TODO read about purescript's profunctor lenses
setDot :: MidiNote -> MidiNote
setDot midiNote = midiNote { hasDot = true }

setFirstTie :: MidiNote -> MidiNote
setFirstTie midiNote = midiNote { hasFirstTie = true }

setEndingTie :: MidiNote -> MidiNote
setEndingTie midiNote = midiNote { hasEndingTie = true }

--REPLACE WITH WRITER MONAD
-- TODO read about ST monad and arrays
midiEventWriter :: List MidiEvent -> List (Tuple MidiEvent Boolean)
midiEventWriter = map (\midiObject -> Tuple midiObject false)

-- TODO kunnen we dit gewoon 'duration' noemen?
calculateDuration :: List (Tuple MidiEvent Boolean) -> List MidiNote
calculateDuration Nil = Nil
calculateDuration midiEvents@(Cons (Tuple (NoteOn midiEvent) isRead) xs) =
  if midiEvent.subtype == "noteOn" then
    Cons { noteNumber   : midiEvent.noteNumber
         , deltaTime    : (accumulateDeltaTime midiEvent.noteNumber 0.0 midiEvents) - midiEvent.deltaTime
         , hasFirstTie  : false
         , hasEndingTie : false
         , hasDot       : false}
    (calculateDuration (replaceBy (==) (noteOff) (toRead noteOff) xs))
  else calculateDuration xs
  where
    noteOff :: Tuple MidiEvent Boolean
    noteOff = fromRight $ findNoteOff midiEvent.noteNumber xs
    toRead :: Tuple MidiEvent Boolean -> Tuple MidiEvent Boolean
    toRead (Tuple midiEvent _) = Tuple midiEvent true
    -- toRead = map (const true)
calculateDuration (Cons x xs) =
  calculateDuration xs

-- TODO sum? lekker kort
accumulateDeltaTime :: Int -> Number -> List (Tuple MidiEvent Boolean) -> Number
accumulateDeltaTime _ _ Nil = 0.0
accumulateDeltaTime noteNumber acc (Cons (Tuple (NoteOn  y) isRead) xs) = accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
accumulateDeltaTime noteNumber acc (Cons (Tuple (NoteOff y) isRead) xs) = if y.noteNumber == noteNumber && not isRead
                                                                          then acc + y.deltaTime
                                                                          else accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
accumulateDeltaTime noteNumber acc (Cons x xs) = accumulateDeltaTime noteNumber acc xs

-- Error to console
findNoteOff :: Int -> List (Tuple MidiEvent Boolean) -> Either String (Tuple MidiEvent Boolean)
findNoteOff _ Nil = Left "No corresponding unread noteOff found."
findNoteOff n (Cons noteOff@(Tuple (NoteOff midiEvent) isRead) xs) =
  if midiEvent.noteNumber == n && not isRead then Right noteOff else findNoteOff n xs

-- findNoteOff n (Cons noteOff@(Tuple (NoteOff midiEvent) false) _) | midiEvent.noteNumber == n =
--   Right noteOff
-- findNoteOff n (Cons         (Tuple (NoteOff midiEvent) _)     notes) =
--   findNoteOff n notes

findNoteOff n (Cons _ xs) = findNoteOff n xs

-- TODO filter!!!#!!## 1 !! LOLOL
-- extractor / view pattern

-- filterNotes :: List MidiEvent -> List MidiEvent
-- filterNotes Nil                     = Nil
-- filterNotes (Cons e@(NoteOn  x) xs) = Cons e (filterNotes xs)
-- filterNotes (Cons e@(NoteOff x) xs) = Cons e (filterNotes xs)
-- filterNotes (Cons _ xs)             = filterNotes xs

-- filterNotes = filter metsj

metsj :: MidiEvent -> Boolean
metsj (NoteOn  _) = true
metsj (NoteOff _) = true
metsj _           = false

getNumerator :: List MidiEvent -> Int
getNumerator (Cons y@(TimeSignature x) xs) = x.numerator
getNumerator (Cons x xs)                   = getNumerator xs

getDeltaTimeNotes :: List MidiEvent -> List Number
getDeltaTimeNotes Nil                     = Nil
getDeltaTimeNotes (Cons y@(NoteOn  x) xs) = Cons x.deltaTime (getDeltaTimeNotes xs)
getDeltaTimeNotes (Cons y@(NoteOff x) xs) = Cons x.deltaTime (getDeltaTimeNotes xs)
getDeltaTimeNotes (Cons x xs)             = getDeltaTimeNotes xs

separateStaff :: List MidiEvent -> List MidiEvent
separateStaff Nil                                         = Nil
separateStaff (Cons y@(NoteOn x) xs)  | x.noteNumber > 59 = Cons y (separateStaff xs)
separateStaff (Cons y@(NoteOff x) xs) | x.noteNumber > 59 = Cons y (separateStaff xs)
separateStaff (Cons x xs)                                 = separateStaff xs

-- Data.Maybe API ff goed bestuderen voor handige funcjes
findStartingTie :: List MidiNote -> List (Maybe Int)
findStartingTie = mapWithIndex (\note i -> if note.hasFirstTie then Just i else Nothing)

midiNoteToVexFlowNote :: Number -> MidiNote -> VexFlowNote
midiNoteToVexFlowNote ticksPerBeat x = { pitch    : [vexNoteToVexFlowPitch $ midiNoteToVexTone x.noteNumber]
                                       , duration : deltaTimeToVexFlowDuration ticksPerBeat x.deltaTime}

midiNoteToVexNote :: Number -> MidiNote -> VexNote
midiNoteToVexNote ticksPerBeat x = { note     : [midiNoteToVexTone x.noteNumber]
                                   , duration : deltaTimeToVexFlowDuration ticksPerBeat x.deltaTime}

midiNoteToVexTone :: Int -> VexTone
midiNoteToVexTone midiNote = { pitch      : fst <<< midiNoteToPartialVexFlowNote $ mod midiNote 12
                             , accidental : snd <<< midiNoteToPartialVexFlowNote $ mod midiNote 12
                             , octave     : midiNoteToOctave midiNote
                             }

-- zie wederom Data.Maybe, iets van catMaybes of filterMaybes ofzo
maybeToInt :: List (Maybe Int) -> List Int
maybeToInt Nil = Nil
maybeToInt (Cons Nothing xs) = (maybeToInt xs)
maybeToInt (Cons x xs)       = Cons (fromJust x) (maybeToInt xs)

-- -- NOTHING? ja fromJust mag je ook niet gebruiken
-- fromJust :: forall a. Maybe a -> a
-- fromJust (Just x) = x

replaceBy :: forall a. (a -> a -> Boolean) -> a -> a -> List a -> List a
replaceBy _ _ _ Nil                     = Nil
replaceBy (==) x z (Cons y ys) | x == y = Cons z ys
replaceBy (==) x z (Cons y ys)          = Cons y (replaceBy (==) x z ys)

toArray :: forall a. List (List a) -> Array (Array a)
toArray lst = toUnfoldable $ map (\x -> toUnfoldable x) lst

-- DISCUSS USING PARTIAL
fromRight :: forall a b. Either a b -> b
fromRight (Right a) = a
