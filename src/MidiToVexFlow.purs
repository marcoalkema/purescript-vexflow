module MidiToVexFlow where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(Nil, Cons), snoc)
import Data.Either (Either(Right, Left))
import Data.Foreign (Foreign, unsafeFromForeign, toForeign, typeOf)
import Data.Tuple (Tuple(Tuple))
import MidiJsTypes (MidiEvent2)
import VexMusic (VexFlowPitch)
import Data.Foldable (foldl)

type VexFoo = {pitch :: Array VexFlowPitch, duration :: Int, subType :: String}

lastNoteDuration :: forall e. Array Foreign -> Eff (console :: CONSOLE, midi :: MidiPlayer.MIDI | e) Unit
lastNoteDuration midiTrack = do
  let songDuration :: Int
      songDuration = foldl (\acc midiEvent -> (showDeltaTime midiEvent) + acc) 0 midiTrack
  ticksPerBeat <- MidiPlayer.getTicksPerBeat
  log $ show ticksPerBeat
  log $ typeOf $ toForeign ticksPerBeat
  let trackDuration = ticksPerBeat + songDuration
  log $ show trackDuration

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

showMidiEvent :: Foreign -> MidiEvent2
showMidiEvent foreignObject = unsafeFromForeign $ fromRight $ Data.Foreign.Index.index 0 foreignObject

fromRight :: forall a b. Either a b -> b
fromRight (Right a) = a

divideIntoMeasures :: Int -> List VexFoo -> List Foreign -> List (List VexFoo)
divideIntoMeasures accumulatedDeltaTime accXs Nil = Nil
divideIntoMeasures accumulatedDeltaTime accXs (Cons x xs) = if accumulatedDeltaTime + (showDeltaTime x) < measure then
                                                               divideIntoMeasures ((showDeltaTime x) + accumulatedDeltaTime) (snoc accXs (showNote x)) xs
                                                             else if accumulatedDeltaTime + (showDeltaTime x) == measure then
                                                                    (Cons (snoc accXs (showNote x)) (divideIntoMeasures 0 Nil xs))
                                                                  else
                                                                    (Cons (snoc accXs (insertNewDeltaTime x lastNoteDeltaTime))) (divideIntoMeasures lastNoteDeltaTime (Cons (insertNewDeltaTime x newFirstNoteDeltaTime) Nil) xs)
    where
      measure               = 480 * 4
      lastNoteDeltaTime     = measure - accumulatedDeltaTime
      newFirstNoteDeltaTime = (showDeltaTime x) - lastNoteDeltaTime

insertNewDeltaTime :: Foreign -> Int -> VexFoo
insertNewDeltaTime midiEvent n =  { pitch    : [show $ showPitchNumber midiEvent]
                                  , duration : n
                                  , subType  : showSubType midiEvent}


-- REPLACE WITH WRITER

toMIDIEvent :: Foreign -> MidiJsTypes.MidiEventFoo
toMIDIEvent midiObject = (unsafeFromForeign $ fromRight $ Data.Foreign.Index.index 0 midiObject).event

midiEventWriter :: List Foreign -> List (Tuple MidiJsTypes.MidiEventFoo Boolean)
midiEventWriter = map (\midiObject -> Tuple (toMIDIEvent midiObject) false)

--ONLY DO IF NOTEON

-- calculateDuration :: List (Tuple MidiJsTypes.MidiEventFoo Boolean) -> List { pitch    :: Int
--                                                                            , duration :: Int}
-- calculateDuration Nil = Nil 
-- calculateDuration (Cons (Tuple (MidiJsTypes.NoteOn midiEvent) isRead) xs) = let noteOff = fromRight $ findNoteOff midiEvent.noteNumber xs
--                                                                             in
--                                                                              if midiEvent.subType == "noteOn" then
--                                                                                Cons { pitch     : midiEvent.noteNumber
--                                                                                     , duration  : (accumulateDeltaTime midiEvent.noteNumber 0 xs)} 
--                                                                                (calculateDuration (replaceBy (==) (noteOff) (toRead (Tuple midiEvent isRead)) xs))
--                                                                             else
--                                                                               calculateDuration xs
-- calculateDuration (Cons x xs) = calculateDuration xs
                                     
--   if (fst x).subType == "noteOn" then
--                                   toList [(fst x)]
--                                 else
--                                   calculateDuration xs

toRead :: Tuple MidiJsTypes.MidiEventFoo Boolean -> Tuple MidiJsTypes.MidiEventFoo Boolean
toRead (Tuple midiEvent read) = Tuple midiEvent true

midiToString :: Tuple MidiJsTypes.MidiEventFoo Boolean -> String
midiToString x = case x of
  (Tuple (MidiJsTypes.NoteOn y) _) -> if y.subType == "noteOn" then
                                        show y.noteNumber
                                        else
                                        show y.deltaTime
  (Tuple (MidiJsTypes.TrackName _) _) -> "hoi1"
  (Tuple (MidiJsTypes.InstrumentName _) _) -> "hoi2"
  (Tuple (MidiJsTypes.TimeSignature _) _) -> "hoi3"
  (Tuple (MidiJsTypes.KeySignature _) _) -> "hoi4"
  (Tuple (MidiJsTypes.Marker _) _) -> "hoi5"
  (Tuple y x) -> show y

midiToString2 :: Tuple MidiJsTypes.MidiEventFoo Boolean -> String
midiToString2 (Tuple (MidiJsTypes.NoteOn x) y) = show x.deltaTime

--EXTRACT MIDIEVENT FROM FOREIGN TYPE IN ORDER TO BE ABLE TO USE IN REPLACEby
accumulateDeltaTime :: Int -> Int -> List (Tuple MidiJsTypes.MidiEventFoo Boolean) -> Int
accumulateDeltaTime _ _ Nil = 0
accumulateDeltaTime noteNumber acc (Cons x xs) = case x of
  Tuple (MidiJsTypes.NoteOn y) isRead  -> accumulateDeltaTime noteNumber (acc + y.deltaTime) xs
  Tuple (MidiJsTypes.NoteOff y) isRead -> if y.noteNumber == noteNumber && isRead == false then
                               acc + y.noteNumber
                             else
                               accumulateDeltaTime noteNumber (acc + y.noteNumber) xs
                               
replaceBy :: forall a. (a -> a -> Boolean) -> a -> a -> List a -> List a
replaceBy _ _ _ Nil = Nil
replaceBy (==) x z (Cons y ys) | x == y = Cons z ys
replaceBy (==) x z (Cons y ys) = Cons y (replaceBy (==) x z ys)

findNoteOff :: Int -> List (Tuple MidiJsTypes.MidiEventFoo Boolean) -> Either String (Tuple MidiJsTypes.MidiEventFoo Boolean)
findNoteOff _ Nil = Left "No corresponding noteOff found."
findNoteOff n (Cons x xs) = case x of
  Tuple (MidiJsTypes.NoteOff y) isRead -> if y.noteNumber == n && isRead == false then
                                      Right x
                                    else
                                      findNoteOff n xs
  _       -> findNoteOff n xs

kip :: {n :: Int
       ,s :: String}
kip = {n : 5, s : "hoi"}
mauw :: {n :: Int, s :: String}
mauw = kip {s = "mauw"}
