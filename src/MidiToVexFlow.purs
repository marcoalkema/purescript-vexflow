module MidiToVexFlow where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(Nil, Cons), snoc)
import Data.Either (Either(Right, Left))
import Data.Foreign (Foreign, unsafeFromForeign, toForeign, typeOf)
import Data.Foreign.Index (prop)
import Data.Tuple
import MidiJsTypes
import VexMusic
import Data.Foldable (foldl)
import Data.Maybe
import VexFlow
import Data.List

type MidiNote = { noteNumber    :: Int
                , deltaTime     :: Number
                , hasFirstTie   :: Boolean
                , hasEndingTie  :: Boolean
                , hasDot        :: Boolean
                }

foreign import unsafeF1 :: Foreign -> MidiEvent

parseMidi :: forall e. Eff (dom :: DOM, midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW | e) (Array Foreign) -> Eff (dom :: DOM, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI | e) Unit
parseMidi dat = do
  midiObjects  <- dat
  ticksPerBeat <- MidiPlayer.getTicksPerBeat
  let sortedData :: Array MidiEvent
      sortedData = map unsafeF1 midiObjects
      numerator  :: Int
      numerator  = getNumerator $ toList sortedData
      midiNotes        :: Array MidiNote
      midiNotes        = toUnfoldable $ Data.List.filter (\x -> x.noteNumber > 0) $ map (quantizeNote ticksPerBeat 0.0) $ calculateDuration $ midiEventWriter $ filterNotes $ toList sortedData
      measuredMidi = toArray $ map (\x -> Data.List.concat <<< setTies ticksPerBeat 3.0 0.0 Nil $ x)$ divideIntoMeasures ticksPerBeat 3 0.0 Nil $ Data.List.filter (\x -> x.noteNumber > 0) $ toList midiNotes
      indexedTies :: Array (Array (Int))
      indexedTies = toArray $ map justToIndex $ map findStartingTie $ map (\x -> Data.List.concat <<< setTies ticksPerBeat 3.0 0.0 Nil $ x)$ divideIntoMeasures ticksPerBeat 3 0.0 Nil $ Data.List.filter (\x -> x.noteNumber > 0) $ toList midiNotes
      toVexFlow = map (\x -> [map (midiNoteToVexFlowNote ticksPerBeat) x]) measuredMidi
  renderNotation toVexFlow indexedTies

toArray :: forall a. List (List a) -> Array (Array a)  
toArray lst = toUnfoldable $ map (\x -> toUnfoldable x) lst

-- DISCUSS USING PARTIAL
fromRight :: forall a b. Either a b -> b
fromRight (Right a) = a

midiNoteToVexFlowNote :: Number -> MidiNote -> VexNote
midiNoteToVexFlowNote ticksPerBeat x = { note     : [midiNoteToVexTone x.noteNumber]
                                       , duration : deltaTimetoDuration ticksPerBeat x.deltaTime}

midiNoteToVexTone :: Int -> VexTone
midiNoteToVexTone midiNote = { pitch      : fst $ midiNoteToPartialVexFlowNote $ mod midiNote 12
                             , accidental : snd $ midiNoteToPartialVexFlowNote $ mod midiNote 12
                             , octave     : midiNoteToOctave midiNote
                             }

--FIX ALL FOR NOTEOFFS
divideIntoMeasures :: Number -> Int -> Number -> List MidiNote -> List MidiNote -> List (List MidiNote)
divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accXs Nil = Nil
divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) = if accumulatedDeltaTime + x.deltaTime < measure then
                                                              divideIntoMeasures ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs x) xs
                                                            else if accumulatedDeltaTime + x.deltaTime == measure then
                                                                   (Cons (snoc accXs x) (divideIntoMeasures ticksPerBeat numerator 0.0 Nil xs))
                                                                 else
                                                                   (Cons (snoc accXs (setFirstTie $ insertNewDeltaTime x lastNoteDeltaTime)))
                                                                   (divideIntoMeasures ticksPerBeat numerator 0.0 Nil (Cons (setEndingTie $ insertNewDeltaTime x newFirstNoteDeltaTime) xs))
--- TPB * Numerator
  where
    measure               = ticksPerBeat * 3.0
    lastNoteDeltaTime     = measure - accumulatedDeltaTime
    newFirstNoteDeltaTime = x.deltaTime - lastNoteDeltaTime

setTies :: Number -> Number -> Number -> List MidiNote -> List MidiNote -> List (List MidiNote)
setTies ticksPerBeat numerator accumulatedDeltaTime accXs Nil = Cons Nil Nil
setTies ticksPerBeat numerator accumulatedDeltaTime accXs (Cons x xs) = if accumulatedDeltaTime + x.deltaTime < ticksPerBeat then
                                                                                                       setTies ticksPerBeat numerator (x.deltaTime + accumulatedDeltaTime) (snoc accXs x) xs
                                                                                                     else if accumulatedDeltaTime + x.deltaTime == ticksPerBeat then
                                                                                                            Cons (snoc accXs x) (setTies ticksPerBeat numerator 0.0 Nil xs)
                                                                                                          else
                                                                                                            (Cons (snoc accXs (setFirstTie $ insertNewDeltaTime x lastNoteDeltaTime)))
                                                                    (setTies ticksPerBeat numerator 0.0 Nil (Cons (setEndingTie $ insertNewDeltaTime x newFirstNoteDeltaTime) xs))
  where
    lastNoteDeltaTime     = ticksPerBeat - accumulatedDeltaTime
    newFirstNoteDeltaTime = x.deltaTime - lastNoteDeltaTime
                                                                                                            
-- setTies divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accXs midiEvent@(Cons x xs) | x == (ticksPerBeat * numerator) = midiEvent
-- setTies divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accXs midiEvent@(Cons x xs) | x.deltaTime + accumulatedDeltaTime == ticksPerBeat = (Cons (snoc accXs x) (divideIntoMeasures ticksPerBeat numerator 0.0 Nil xs))
-- setTies divideIntoMeasures ticksPerBeat numerator accumulatedDeltaTime accXs midiEvent@(Cons x xs) | x.deltaTime + accumulatedDeltaTime > ticksPerBeat = 

setFirstTie :: MidiNote -> MidiNote
setFirstTie midiNote =  { noteNumber   : midiNote.noteNumber
                        , deltaTime    : midiNote.deltaTime
                        , hasFirstTie  : true
                        , hasEndingTie : false
                        , hasDot       : false}

setEndingTie :: MidiNote -> MidiNote
setEndingTie midiNote =  { noteNumber   : midiNote.noteNumber
                         , deltaTime    : midiNote.deltaTime
                         , hasFirstTie  : false
                         , hasEndingTie : true
                         , hasDot       : false}
                            
insertNewDeltaTime :: MidiNote -> Number -> MidiNote
insertNewDeltaTime midiNote n =  { noteNumber   : midiNote.noteNumber
                                 , deltaTime    : n
                                 , hasFirstTie  : false
                                 , hasEndingTie : false
                                 , hasDot       : false}

--REPLACE WITH WRITER
midiEventWriter :: List MidiEvent -> List (Tuple MidiJsTypes.MidiEvent Boolean)
midiEventWriter = map (\midiObject -> Tuple midiObject false)

toRead :: Tuple MidiJsTypes.MidiEvent Boolean -> Tuple MidiJsTypes.MidiEvent Boolean
toRead (Tuple midiEvent read) = Tuple midiEvent true

calculateDuration :: List (Tuple MidiJsTypes.MidiEvent Boolean) -> List MidiNote
calculateDuration Nil = Nil 
calculateDuration midiEvents@(Cons (Tuple (MidiJsTypes.NoteOn midiEvent) isRead) xs) = let noteOff :: Tuple MidiEvent Boolean
                                                                                           noteOff =  fromRight $ findNoteOff midiEvent.noteNumber xs
                                                                            in
                                                                             if midiEvent.subtype == "noteOn" then
                                                                               Cons { noteNumber     : midiEvent.noteNumber
                                                                                    , deltaTime  : (accumulateDeltaTime midiEvent.noteNumber 0.0 midiEvents) - midiEvent.deltaTime
                                                                                    , hasFirstTie   : false
                                                                                    , hasEndingTie  : false
                                                                                    , hasDot        : false}
                                                                               (calculateDuration (replaceBy (==) (noteOff) (toRead noteOff) xs))
                                                                            else
                                                                              calculateDuration xs
calculateDuration (Cons x xs)                                                        = calculateDuration xs

accumulateDeltaTime :: Int -> Number -> List (Tuple MidiJsTypes.MidiEvent Boolean) -> Number
accumulateDeltaTime _ _ Nil = 0.0
accumulateDeltaTime noteNumber acc (Cons (Tuple (MidiJsTypes.NoteOn y) isRead) xs)  = accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
accumulateDeltaTime noteNumber acc (Cons (Tuple (MidiJsTypes.NoteOff y) isRead) xs) = if y.noteNumber == noteNumber && isRead == false then
                               acc + y.deltaTime
                             else
                               accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
                               
replaceBy :: forall a. (a -> a -> Boolean) -> a -> a -> List a -> List a
replaceBy _ _ _ Nil                     = Nil
replaceBy (==) x z (Cons y ys) | x == y = Cons z ys
replaceBy (==) x z (Cons y ys)          = Cons y (replaceBy (==) x z ys)

findNoteOff :: Int -> List (Tuple MidiJsTypes.MidiEvent Boolean) -> Either String (Tuple MidiJsTypes.MidiEvent Boolean)
findNoteOff _ Nil = Left "No corresponding unread noteOff found."
findNoteOff n (Cons noteOff@(Tuple (MidiJsTypes.NoteOff midiEvent) isRead) xs) = if midiEvent.noteNumber == n && isRead == false then
                                                                   Right noteOff
                                                                 else
                                                                   findNoteOff n xs
findNoteOff n (Cons _ xs)                                                      = findNoteOff n xs

-- quantizeTriplets

quantizeNote :: Number -> Number -> MidiNote -> MidiNote
quantizeNote ticksPerBeat acc event | event.deltaTime < (acc * ticksPerBeat / 4.0)                                                                          = event {deltaTime = acc * (ticksPerBeat / 4.0)}
quantizeNote ticksPerBeat acc event | event.deltaTime > (acc * ticksPerBeat / 4.0) && event.deltaTime < (acc * (ticksPerBeat / 4.0) + (ticksPerBeat / 8.0)) = event {deltaTime = acc * (ticksPerBeat / 4.0)}
quantizeNote ticksPerBeat acc event | event.deltaTime > (acc * ticksPerBeat / 4.0) && event.deltaTime > (acc * (ticksPerBeat / 4.0) + (ticksPerBeat / 8.0)) = quantizeNote ticksPerBeat (acc + 1.0) event
quantizeNote ticksPerBeat acc event                                                                                                                         = event

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

findStartingTie :: List MidiNote -> List (Maybe Int)
findStartingTie Nil = Nil
findStartingTie midiNotes@(Cons x xs) = mapWithIndex (\x n -> if x.hasFirstTie == true then Just n else Nothing) midiNotes

justToIndex :: List (Maybe Int) -> List Int
justToIndex Nil = Nil
justToIndex (Cons x xs) | x == Nothing = (justToIndex xs)
justToIndex (Cons x xs) = Cons (fromJust x) (justToIndex xs)

fromJust :: forall a. Maybe a -> a
fromJust (Just x) = x
