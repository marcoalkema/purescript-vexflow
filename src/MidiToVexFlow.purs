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

foreign import unsafeF1 :: Foreign -> MidiEvent

parseMidi :: forall e. Eff (midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW | e) (Array Foreign) -> Eff (vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI | e) Unit
parseMidi dat = do
  midiData      <- dat
  ticksPerBeat  <- getTicksPerBeat
  let safeData  = toList $ map unsafeF1 midiData
      numerator = getNumerator safeData
      midiNotes = filter (\x -> x.noteNumber > 0)
                  <<< map (quantizeNote ticksPerBeat 0.0)
                  <<< calculateDuration
                  <<< midiEventWriter
                  <<< filterNotes
                  $ toList safeData
      measuredMidi = map (concat <<< setTies ticksPerBeat 4.0 0.0 Nil)
                     <<< map (concat <<< setDots ticksPerBeat 4.0 0.0 Nil)
                     $ divideIntoMeasures ticksPerBeat 4.0 0.0 Nil midiNotes
      indexedTies = toArray <<< map maybeToInt $ map findStartingTie measuredMidi
      indexedBeams :: Array (Array (Array Int))
      indexedBeams = toUnfoldable <<< map (toArray <<< beamsIndex Nil) $ map (eighthsIndex ticksPerBeat 0) measuredMidi
      vexFlowNotes = map (\x -> [map (midiNoteToVexFlowNote ticksPerBeat) x]) $ toArray measuredMidi
      vexNotes     = map (\x -> [map (midiNoteToVexNote ticksPerBeat) x]) $ toArray measuredMidi
  renderNotation vexFlowNotes vexNotes indexedTies indexedBeams

--FIX ALL FOR NOTEOFFS/RESTS
divideIntoMeasures :: Number -> Number -> Number -> List MidiNote -> List MidiNote -> List (List MidiNote)
divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accXs Nil = Nil
divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) = if accumulatedDeltaTime + x.deltaTime < measure then
                                                                                     divideIntoMeasures ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs x) xs
                                                                                   else if accumulatedDeltaTime + x.deltaTime == measure then
                                                                                          (Cons (snoc accXs x) (divideIntoMeasures ticksPerBeat numerator 0.0 Nil xs))
                                                                                        else
                                                                                          (Cons (snoc accXs (setFirstTie $ insertNewDeltaTime lastNoteDeltaTime x)))
                                                                                          (divideIntoMeasures ticksPerBeat numerator 0.0 Nil (Cons (setEndingTie $ insertNewDeltaTime newFirstNoteDeltaTime x) xs))
  where
    measure               = ticksPerBeat * numerator
    lastNoteDeltaTime     = measure - accumulatedDeltaTime
    newFirstNoteDeltaTime = x.deltaTime - lastNoteDeltaTime
                            
setTies :: Number -> Number -> Number -> List MidiNote -> List MidiNote -> List (List MidiNote)
setTies ticksPerBeat numerator accumulatedDeltaTime accXs Nil = Cons accXs Nil
setTies ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) | accumulatedDeltaTime + x.deltaTime < ticksPerBeat = setTies ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs x) xs
setTies ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) | accumulatedDeltaTime + x.deltaTime == ticksPerBeat = Cons (snoc accXs x) (setTies ticksPerBeat numerator 0.0 Nil xs)
setTies ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) | (accumulatedDeltaTime + x.deltaTime > ticksPerBeat)
                                                                        && (x.hasDot == true) = if accumulatedDeltaTime == 0.0 then
                                                                                                  Cons (snoc accXs x) (setTies ticksPerBeat numerator 120.0 Nil xs)
                                                                                                else
                                                                                                  Cons (snoc accXs x) (setTies ticksPerBeat numerator 0.0 Nil xs)
setTies ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) | (accumulatedDeltaTime + x.deltaTime > ticksPerBeat)
                                                                        && (x.hasDot == false) = (Cons (snoc accXs (setFirstTie $ insertNewDeltaTime lastNoteDeltaTime x))) (setTies ticksPerBeat numerator 0.0 Nil (Cons (setEndingTie $ insertNewDeltaTime newFirstNoteDeltaTime x) xs))
  where
    lastNoteDeltaTime     = ticksPerBeat - accumulatedDeltaTime
    newFirstNoteDeltaTime = x.deltaTime - lastNoteDeltaTime

insertNewDeltaTime :: Number -> MidiNote -> MidiNote
insertNewDeltaTime n midiNote =  { noteNumber   : midiNote.noteNumber
                                 , deltaTime    : n
                                 , hasFirstTie  : false
                                 , hasEndingTie : false
                                 , hasDot       : false }

setDots :: Number -> Number -> Number -> List MidiNote -> List MidiNote -> List (List MidiNote)
setDots ticksPerBeat numerator accumulatedDeltaTime accXs Nil = Cons accXs Nil
setDots ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) | (accumulatedDeltaTime == 0.0 && x.deltaTime == ticksPerBeat * 1.5 )                       ||
                                                                        (accumulatedDeltaTime == ticksPerBeat / 2.0 && x.deltaTime == ticksPerBeat * 1.5)         ||
                                                                        (accumulatedDeltaTime == (ticksPerBeat / 2.0) * 4.0 && x.deltaTime == ticksPerBeat * 1.5) ||
                                                                        (accumulatedDeltaTime == (ticksPerBeat / 2.0) * 5.0 && x.deltaTime == ticksPerBeat * 1.5) = setDots ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs $ setDot x) xs
setDots ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) = setDots ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs x) xs

eighthsIndex :: Number -> Int -> List MidiNote -> List Int
eighthsIndex ticksPerBeat acc Nil = Nil
eighthsIndex ticksPerBeat acc (Cons x xs) | x.deltaTime > ticksPerBeat / 2.0  = eighthsIndex ticksPerBeat (acc + 1) xs
eighthsIndex ticksPerBeat acc (Cons x xs) | x.deltaTime <= ticksPerBeat / 2.0 = Cons acc $ eighthsIndex ticksPerBeat (acc + 1) xs

beamsIndex :: List Int -> List Int -> List (List Int)
beamsIndex accXs Nil                               = Cons accXs Nil
beamsIndex accXs (Cons x Nil)                      = Cons (snoc accXs x) Nil
beamsIndex accXs (Cons x (Cons y ys)) | x /= y - 1 = Cons accXs (beamsIndex Nil (Cons y ys))
beamsIndex accXs (Cons x (Cons y ys)) | x == y - 1 = beamsIndex (snoc accXs x) (Cons y ys)

setDot :: MidiNote -> MidiNote
setDot midiNote = midiNote { hasDot = true }
                   
setFirstTie :: MidiNote -> MidiNote
setFirstTie midiNote = midiNote { hasFirstTie = true }

setEndingTie :: MidiNote -> MidiNote
setEndingTie midiNote = midiNote { hasEndingTie = true }

--REPLACE WITH WRITER MONAD
midiEventWriter :: List MidiEvent -> List (Tuple MidiJsTypes.MidiEvent Boolean)
midiEventWriter = map (\midiObject -> Tuple midiObject false)

calculateDuration :: List (Tuple MidiJsTypes.MidiEvent Boolean) -> List MidiNote
calculateDuration Nil = Nil 
calculateDuration midiEvents@(Cons (Tuple (MidiJsTypes.NoteOn midiEvent) isRead) xs) = if midiEvent.subtype == "noteOn" then
                                                                                         Cons { noteNumber   : midiEvent.noteNumber
                                                                                              , deltaTime    : (accumulateDeltaTime midiEvent.noteNumber 0.0 midiEvents) - midiEvent.deltaTime
                                                                                              , hasFirstTie  : false
                                                                                              , hasEndingTie : false
                                                                                              , hasDot       : false}
                                                                                         (calculateDuration (replaceBy (==) (noteOff) (toRead noteOff) xs))
                                                                                       else
                                                                                         calculateDuration xs
  where
    noteOff :: Tuple MidiEvent Boolean
    noteOff =  fromRight $ findNoteOff midiEvent.noteNumber xs
    toRead :: Tuple MidiJsTypes.MidiEvent Boolean -> Tuple MidiJsTypes.MidiEvent Boolean
    toRead (Tuple midiEvent read) = Tuple midiEvent true
calculateDuration (Cons x xs)                                                        = calculateDuration xs
                                                                                       
accumulateDeltaTime :: Int -> Number -> List (Tuple MidiJsTypes.MidiEvent Boolean) -> Number
accumulateDeltaTime _ _ Nil = 0.0
accumulateDeltaTime noteNumber acc (Cons (Tuple (MidiJsTypes.NoteOn y) isRead) xs)  = accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
accumulateDeltaTime noteNumber acc (Cons (Tuple (MidiJsTypes.NoteOff y) isRead) xs) = if y.noteNumber == noteNumber && isRead == false then
                                                                                        acc + y.deltaTime
                                                                                      else
                                                                                        accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
accumulateDeltaTime noteNumber acc (Cons x xs) = accumulateDeltaTime noteNumber acc xs

-- Error to console
findNoteOff :: Int -> List (Tuple MidiJsTypes.MidiEvent Boolean) -> Either String (Tuple MidiJsTypes.MidiEvent Boolean)
findNoteOff _ Nil = Left "No corresponding unread noteOff found."
findNoteOff n (Cons noteOff@(Tuple (MidiJsTypes.NoteOff midiEvent) isRead) xs) = if midiEvent.noteNumber == n && isRead == false then
                                                                                   Right noteOff
                                                                                 else
                                                                                   findNoteOff n xs
findNoteOff n (Cons _ xs)                                                      = findNoteOff n xs

filterNotes :: List MidiEvent -> List MidiEvent
filterNotes Nil                      = Nil
filterNotes (Cons y@(NoteOn x) xs)   = Cons y (filterNotes xs)
filterNotes (Cons y@(NoteOff x) xs)  = Cons y (filterNotes xs)
filterNotes (Cons x xs)              = filterNotes xs

getNumerator :: List MidiEvent -> Int
getNumerator (Cons y@(TimeSignature x) xs) = x.numerator
getNumerator (Cons x xs)                   = getNumerator xs

getDeltaTimeNotes :: List MidiEvent -> List Number
getDeltaTimeNotes Nil                     = Nil
getDeltaTimeNotes (Cons y@(NoteOn x) xs)  = Cons x.deltaTime (getDeltaTimeNotes xs)
getDeltaTimeNotes (Cons y@(NoteOff x) xs) = Cons x.deltaTime (getDeltaTimeNotes xs)
getDeltaTimeNotes (Cons x xs)             = getDeltaTimeNotes xs

separateStaff :: List MidiEvent -> List MidiEvent
separateStaff Nil                                         = Nil
separateStaff (Cons y@(NoteOn x) xs)  | x.noteNumber > 59 = Cons y (separateStaff xs)
separateStaff (Cons y@(NoteOff x) xs) | x.noteNumber > 59 = Cons y (separateStaff xs)
separateStaff (Cons x xs)                                 = separateStaff xs

findStartingTie :: List MidiNote -> List (Maybe Int)
findStartingTie Nil = Nil
findStartingTie midiNotes@(Cons x xs) = mapWithIndex (\x n -> if x.hasFirstTie == true then Just n else Nothing) midiNotes

midiNoteToVexFlowNote :: Number -> MidiNote -> VexFlowNote
midiNoteToVexFlowNote ticksPerBeat x = { pitch     : [vexNoteToVexFlowPitch $ midiNoteToVexTone x.noteNumber]
                                       , duration : deltaTimeToVexFlowDuration ticksPerBeat x.deltaTime}

midiNoteToVexNote :: Number -> MidiNote -> VexNote
midiNoteToVexNote ticksPerBeat x = { note     : [midiNoteToVexTone x.noteNumber]
                                   , duration : deltaTimeToVexFlowDuration ticksPerBeat x.deltaTime}

midiNoteToVexTone :: Int -> VexTone
midiNoteToVexTone midiNote = { pitch      : fst $ midiNoteToPartialVexFlowNote $ mod midiNote 12
                             , accidental : snd $ midiNoteToPartialVexFlowNote $ mod midiNote 12
                             , octave     : midiNoteToOctave midiNote
                             }

maybeToInt :: List (Maybe Int) -> List Int
maybeToInt Nil = Nil
maybeToInt (Cons x xs) | x == Nothing = (maybeToInt xs)
maybeToInt (Cons x xs)                = Cons (fromJust x) (maybeToInt xs)

-- NOTHING?
fromJust :: forall a. Maybe a -> a
fromJust (Just x) = x

replaceBy :: forall a. (a -> a -> Boolean) -> a -> a -> List a -> List a
replaceBy _ _ _ Nil                     = Nil
replaceBy (==) x z (Cons y ys) | x == y = Cons z ys
replaceBy (==) x z (Cons y ys)          = Cons y (replaceBy (==) x z ys)

toArray :: forall a. List (List a) -> Array (Array a)  
toArray lst = toUnfoldable $ map (\x -> toUnfoldable x) lst

-- DISCUSS USING PARTIAL
fromRight :: forall a b. Either a b -> b
fromRight (Right a) = a
