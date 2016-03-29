module MidiToVexFlow where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(Nil, Cons), snoc)
import Data.Either (Either(Right, Left))
import Data.Foreign (Foreign, unsafeFromForeign, toForeign, typeOf)
import Data.Foreign.Index (prop)
import Data.Tuple (Tuple(Tuple))
import MidiJsTypes
import VexMusic (VexFlowPitch)
import Data.Foldable (foldl)

type MidiNote = { noteNumber    :: Int
                , deltaTime     :: Int}

foreign import unsafeF1 :: Foreign -> MidiEventFoo

-- DISCUSS USING PARTIAL
fromRight :: forall a b. Either a b -> b
fromRight (Right a) = a

--FIX ALL FOR NOTEOFFS
divideIntoMeasures :: Int -> List MidiNote -> List MidiNote -> List (List MidiNote)
divideIntoMeasures accumulatedDeltaTime accXs Nil = Nil
divideIntoMeasures accumulatedDeltaTime accXs (Cons x xs) = if accumulatedDeltaTime + x.deltaTime < measure then
                                                               divideIntoMeasures (x.deltaTime + accumulatedDeltaTime) (snoc accXs x) xs
                                                             else if accumulatedDeltaTime + (x.deltaTime) == measure then
                                                                    (Cons (snoc accXs x) (divideIntoMeasures 0 Nil xs))
                                                                  else
                                                                    (Cons (snoc accXs (insertNewDeltaTime x lastNoteDeltaTime)))
                                                                    (divideIntoMeasures 0 Nil (Cons (insertNewDeltaTime x newFirstNoteDeltaTime) xs))
    where
      measure               = 480 * 4
      lastNoteDeltaTime     = measure - accumulatedDeltaTime
      newFirstNoteDeltaTime = x.deltaTime - lastNoteDeltaTime

insertNewDeltaTime :: MidiNote -> Int -> MidiNote
insertNewDeltaTime midiNote n =  { noteNumber : midiNote.noteNumber
                                 , deltaTime  : n }

--REPLACE WITH WRITER
midiEventWriter :: List MidiEventFoo -> List (Tuple MidiJsTypes.MidiEventFoo Boolean)
midiEventWriter = map (\midiObject -> Tuple midiObject false)

toRead :: Tuple MidiJsTypes.MidiEventFoo Boolean -> Tuple MidiJsTypes.MidiEventFoo Boolean
toRead (Tuple midiEvent read) = Tuple midiEvent true

calculateDuration :: List (Tuple MidiJsTypes.MidiEventFoo Boolean) -> List MidiNote
calculateDuration Nil = Nil 
calculateDuration midiEvents@(Cons (Tuple (MidiJsTypes.NoteOn midiEvent) isRead) xs) = let noteOff :: Tuple MidiEventFoo Boolean
                                                                                           noteOff =  fromRight $ findNoteOff midiEvent.noteNumber xs
                                                                            in
                                                                             if midiEvent.subtype == "noteOn" then
                                                                               Cons { noteNumber     : midiEvent.noteNumber
                                                                                    , deltaTime  : (accumulateDeltaTime midiEvent.noteNumber 0 midiEvents) - midiEvent.deltaTime} 
                                                                               (calculateDuration (replaceBy (==) (noteOff) (toRead noteOff) xs))
                                                                            else
                                                                              calculateDuration xs
calculateDuration (Cons x xs)                                                        = calculateDuration xs

accumulateDeltaTime :: Int -> Int -> List (Tuple MidiJsTypes.MidiEventFoo Boolean) -> Int
accumulateDeltaTime _ _ Nil = 0
accumulateDeltaTime noteNumber acc (Cons (Tuple (MidiJsTypes.NoteOn y) isRead) xs)  = accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
accumulateDeltaTime noteNumber acc (Cons (Tuple (MidiJsTypes.NoteOff y) isRead) xs) = if y.noteNumber == noteNumber && isRead == false then
                               acc + y.deltaTime
                             else
                               accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
                               
replaceBy :: forall a. (a -> a -> Boolean) -> a -> a -> List a -> List a
replaceBy _ _ _ Nil                     = Nil
replaceBy (==) x z (Cons y ys) | x == y = Cons z ys
replaceBy (==) x z (Cons y ys)          = Cons y (replaceBy (==) x z ys)

findNoteOff :: Int -> List (Tuple MidiJsTypes.MidiEventFoo Boolean) -> Either String (Tuple MidiJsTypes.MidiEventFoo Boolean)
findNoteOff _ Nil = Left "No corresponding unread noteOff found."
findNoteOff n (Cons noteOff@(Tuple (MidiJsTypes.NoteOff midiEvent) isRead) xs) = if midiEvent.noteNumber == n && isRead == false then
                                                                   Right noteOff
                                                                 else
                                                                   findNoteOff n xs
findNoteOff n (Cons _ xs)                                                      = findNoteOff n xs
