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
import VexMusic
import Data.Foldable (foldl)
import Data.Maybe
import VexFlow
import Data.List
import Data.List (concat, filter)
import Quantizer

type TicksPerBeat = Number
type Numerator = Number
type DeltaTime = Number

foreign import unsafeF1 :: Foreign -> MidiEvent

renderMidi :: forall e. Canvas -> Array Foreign -> Eff (vexFlow :: VEXFLOW, midi :: MIDI | e) Unit
renderMidi canvas midiData = do
  ticksPerBeat  <- getTicksPerBeat
  let safeData  = toList $ map unsafeF1 midiData
      numerator = getNumerator safeData
      midiNotes = filter (\x -> x.noteNumber > 0)
                  <<< map (quantizeNote ticksPerBeat 0.0)
                  <<< calculateDuration
                  <<< map (\midiObject -> Tuple midiObject false) -- midiEventWriter
                  <<< filter metsj
                  $ toList safeData
      measuredMidi = map ((concat <<< setDots ticksPerBeat 4.0 0.0 Nil) <<<
                          (concat <<< setTies ticksPerBeat 4.0 0.0 Nil))
                     $ divideIntoMeasures ticksPerBeat 4.0 0.0 Nil midiNotes
      indexedTies = toArray $ map (maybeToInt <<< findStartingTie) measuredMidi
      indexedBeams :: Array (Array (Array Int))
      indexedBeams = toUnfoldable <<< map (toArray <<< beamsIndex Nil) $ map (eighthsIndex ticksPerBeat 0) measuredMidi
      vexFlowNotes = map (\x -> [map (midiNoteToVexFlowNote ticksPerBeat) x]) $ toArray measuredMidi
      vexNotes     = map (\x -> [map (midiNoteToVexNote ticksPerBeat) x]) $ toArray measuredMidi
  renderNotation canvas vexFlowNotes vexNotes indexedTies indexedBeams

-- TODO expliciete recursie wegwerken (fold ofzo)
--FIX ALL FOR NOTEOFFS/RESTS
divideIntoMeasures :: TicksPerBeat -> Numerator -> DeltaTime -> List MidiNote -> List MidiNote -> List (List MidiNote)
divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accNotes Nil = Nil
divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accNotes (Cons note notes) =
  if dt < measure then
         divideIntoMeasures ticksPerBeat numerator dt (snoc accNotes note) notes
  else if dt == measure then
         (Cons (snoc accNotes note) (divideIntoMeasures ticksPerBeat numerator 0.0 Nil notes))
  else
         (Cons (snoc accNotes (setFirstTie $ insertNewDeltaTime lastNoteDeltaTime note)))
         (divideIntoMeasures ticksPerBeat numerator 0.0 Nil (Cons (setEndingTie $ insertNewDeltaTime newFirstNoteDeltaTime note) notes))
  where
    measure               = ticksPerBeat * numerator
    lastNoteDeltaTime     = measure - accumulatedDeltaTime
    newFirstNoteDeltaTime = note.deltaTime - lastNoteDeltaTime
    dt                    = accumulatedDeltaTime + note.deltaTime

-- duplicatie elimineren mbv if-then ipv guards

setTies :: TicksPerBeat -> Numerator -> DeltaTime -> List MidiNote -> List MidiNote -> List (List MidiNote)
setTies ticksPerBeat numerator accumulatedDeltaTime accNotes Nil = Cons accNotes Nil
setTies ticksPerBeat numerator accumulatedDeltaTime accNotes (Cons note notes) =
  if dt < ticksPerBeat then
         setTies ticksPerBeat numerator dt (snoc accNotes note) notes
  else if dt == ticksPerBeat || (dt > ticksPerBeat && note.hasDot) then
         Cons (snoc accNotes note) (setTies ticksPerBeat numerator (if accumulatedDeltaTime == 0.0 then quarterNote else 0.0) Nil notes)
  else
         Cons (snoc accNotes (setFirstTie $ insertNewDeltaTime lastNoteDeltaTime note)) (setTies ticksPerBeat numerator 0.0 Nil (Cons (setEndingTie $ insertNewDeltaTime newFirstNoteDeltaTime note) notes))
  where
    lastNoteDeltaTime     = ticksPerBeat - accumulatedDeltaTime
    newFirstNoteDeltaTime = note.deltaTime - lastNoteDeltaTime
    dt                    = accumulatedDeltaTime + note.deltaTime
    quarterNote           = 120.0

insertNewDeltaTime :: Number -> MidiNote -> MidiNote
insertNewDeltaTime n midiNote = { noteNumber   : midiNote.noteNumber
                                , deltaTime    : n
                                , hasFirstTie  : false
                                , hasEndingTie : false
                                , hasDot       : false }

-- TODO underscrores jwt
setDots :: TicksPerBeat -> Numerator -> DeltaTime -> List MidiNote -> List MidiNote -> List (List MidiNote)
setDots _            _         _                    accXs Nil = Cons accXs Nil
setDots ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) | (accumulatedDeltaTime == 0.0 && x.deltaTime == ticksPerBeat * 1.5 )                       ||
                                                                        (accumulatedDeltaTime == ticksPerBeat / 2.0 && x.deltaTime == ticksPerBeat * 1.5)         ||
                                                                        (accumulatedDeltaTime == (ticksPerBeat / 2.0) * 4.0 && x.deltaTime == ticksPerBeat * 1.5) ||
                                                                        (accumulatedDeltaTime == (ticksPerBeat / 2.0) * 5.0 && x.deltaTime == ticksPerBeat * 1.5) = setDots ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs $ setDot x) xs
setDots ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) = setDots ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs x) xs

-- TODO filter ipv expl recursie
eighthsIndex :: TicksPerBeat -> Int -> List MidiNote -> List Int
eighthsIndex ticksPerBeat i Nil = Nil
eighthsIndex ticksPerBeat i (Cons x xs) | x.deltaTime >  ticksPerBeat / 2.0 =          eighthsIndex ticksPerBeat (i + 1) xs
eighthsIndex ticksPerBeat i (Cons x xs) | x.deltaTime <= ticksPerBeat / 2.0 = Cons i $ eighthsIndex ticksPerBeat (i + 1) xs

beamsIndex :: List Int -> List Int -> List (List Int)
beamsIndex accXs Nil                               = Cons accXs          Nil
beamsIndex accXs (Cons x Nil)                      = Cons (snoc accXs x) Nil
beamsIndex accXs (Cons x (Cons y ys)) | x /= y - 1 = Cons accXs          (beamsIndex Nil (Cons y ys))
beamsIndex accXs (Cons x (Cons y ys)) | x == y - 1 = beamsIndex (snoc accXs x) (Cons y ys)

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
