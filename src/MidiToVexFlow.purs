module MidiToVexFlow where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff
import Control.Monad.Eff.Console (CONSOLE, log)
import ColorNotation
import Data.Either (Either(Right, Left))
import Data.Foreign
import Data.Foldable
import Data.Foldable (foldl)
import Data.Int
import Data.List
import Data.List (List(Nil, Cons), snoc, concat, filter)
import Data.Maybe
import Data.Tuple
import MidiJS as MidiPlayer2
import MidiJsTypes
import MidiJsTypes (MidiNote)
import MidiPlayer
import Signal
import Quantizer
import VexFlow
import VexMusic

type TicksPerBeat   = Number
type Numerator      = Int
type DeltaTime      = Number
type TicksPerBeat   = Number
type NoteNumber     = Int

foreign import unsafeF1 :: UnsafeMidiData -> MidiEvent

type VexFlowResult = {
  vexFlowNotes :: VexFlowMusic
, vexNotes     :: VexMusic
, indexedTies  :: Array (Array TieIndex)
, indexedBeams :: Array (Array (Array BeamIndex))
, numerator    :: Int
}

renderMidiPure :: Array UnsafeMidiData -> TicksPerBeat -> VexFlowResult
renderMidiPure dat tpb = do
  let midiEvents = toList $ map unsafeF1 dat
      numerator  = getNumerator midiEvents
      midiNotes  = filter (\x -> x.noteNumber > 0)
                   <<< map (quantizeNote tpb 0.0)
                   <<< duration
                   <<< map (\midiObject -> Tuple midiObject false) -- midiEventWriter
                   <<< filter filterNotes
                   $ toList midiEvents
      measuredMidi = map (setTies tpb numerator <<< setDots tpb numerator)
                     $ divideIntoMeasures tpb numerator 0.0 Nil midiNotes
      indexedTies  = toArray $ map findStartingTies measuredMidi
      indexedBeams :: Array (Array (Array BeamIndex))
      indexedBeams = toUnfoldable $ map ((toArray <<< beamsIndex tpb Nil) <<< (eighthsIndex' tpb)) measuredMidi
      vexFlowNotes = map (\x -> [map (midiNoteToVexFlowNote tpb) x]) $ toArray measuredMidi
      vexNotes     = map (\x -> [map (midiNoteToVexNote tpb) x]) $ toArray measuredMidi
  { vexFlowNotes, vexNotes, indexedTies, indexedBeams, numerator }

-- TODO glue, eliminate
renderMidi :: forall e. Canvas -> Int -> Array (Array (Array Boolean)) -> VexFlowResult -> Eff (vexFlow :: VEXFLOW, midi :: MIDI | e) Unit
renderMidi canvas colorIndex notationHasColor x = do
  renderNotation canvas x.vexFlowNotes x.vexNotes x.indexedTies x.indexedBeams x.numerator (setHasColor colorIndex notationHasColor)

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
         (Cons (snoc accNotes (createStartTie note)))
         (divideIntoMeasures ticksPerBeat numerator 0.0 Nil (Cons (createEndingTie note) notes))
  where
    measure               = ticksPerBeat * (toNumber numerator)
    lastNoteDeltaTime     = measure - accumulatedDeltaTime
    newFirstNoteDeltaTime = note.deltaTime - lastNoteDeltaTime
    dt                    = accumulatedDeltaTime + note.deltaTime
    createStartTie        = setFirstTie <<< insertNewDeltaTime lastNoteDeltaTime
    createEndingTie       = setEndingTie <<< insertNewDeltaTime newFirstNoteDeltaTime

setFirstTie :: MidiNote -> MidiNote
setFirstTie midiNote = midiNote { hasFirstTie = true }

setEndingTie :: MidiNote -> MidiNote
setEndingTie midiNote = midiNote { hasEndingTie = true }

-- Fix case for position 1.5tpb && deltaTime > 1.5tpb
-- Fix case for position 0.5tpb && deltaTime > 1.5tpb
-- Fix case for position 2.5tpb && deltaTime > 1.5tpb
setTies :: TicksPerBeat -> Numerator -> List MidiNote -> List MidiNote
setTies tpb num notes = (_.midiNotes) $ foldl setTie {baseNotes : notes, accDt : 0.0, midiNotes : Nil } notes
  where
    setTie b n = let dt                    = b.accDt + n.deltaTime
                     tailBaseNotes         = fromMaybe Nil $ tail b.baseNotes
                     hasDot n              = if n.deltaTime == 1.5 * tpb then
                                               n { hasDot = true }
                                             else
                                               n
                     newNote n dt ft et hd = { noteNumber   : n
                                             , deltaTime    : dt
                                             , hasFirstTie  : ft
                                             , hasEndingTie : et
                                             , hasDot       : hd }
             in
              if (b.accDt == tpb * 0.5 && n.deltaTime >= tpb && n.deltaTime /= tpb * 1.5)
              || (b.accDt == tpb * 2.5 && n.deltaTime >= tpb && n.deltaTime /= tpb * 1.5)  then
                  { baseNotes    : tailBaseNotes
                  , accDt        : dt
                  , midiNotes    : snoc (snoc b.midiNotes
                                         $ newNote n.noteNumber (n.deltaTime / 2.0) true false false)
                                   $ newNote n.noteNumber (n.deltaTime / 2.0) false true false }
              else if (b.accDt == tpb * 1.5 && n.deltaTime >= tpb) then
                  { baseNotes    : tailBaseNotes
                  , accDt        : dt
                  , midiNotes    : snoc (snoc b.midiNotes
                                         $ newNote n.noteNumber (tpb * 0.5) true false false)
                                   <<< hasDot $ newNote n.noteNumber ((-) n.deltaTime $ tpb * 0.5) false true false }
              else
                {baseNotes : fromMaybe Nil $ tail b.baseNotes, accDt : dt, midiNotes : snoc b.midiNotes n }
                     
insertNewDeltaTime :: DeltaTime -> MidiNote -> MidiNote
insertNewDeltaTime n midiNote = midiNote { deltaTime = n }

-- Note :: postion 0.5tpb + deltaTime = 3.5tpb???????
setDots :: TicksPerBeat -> Numerator -> List MidiNote -> (List MidiNote)
setDots tpb num notes = mapWithIndex checkPosition notes
  where
    checkPosition :: MidiNote -> Int -> MidiNote
    checkPosition n i | (currentPosition i == 0.0               && n.deltaTime == tpb * 1.5) ||
                        (currentPosition i == tpb / 2.0         && n.deltaTime == tpb * 1.5) ||
                        (currentPosition i == (tpb / 2.0) * 4.0 && n.deltaTime == tpb * 1.5) ||
                        (currentPosition i == (tpb / 2.0) * 5.0 && n.deltaTime == tpb * 1.5)  = setDot n
    checkPosition n _  = n
    setDot :: MidiNote -> MidiNote
    setDot midiNote = midiNote { hasDot = true }
    currentPosition :: Int -> DeltaTime
    currentPosition i = fromMaybe 0.0 <<< flip index i $ sumDeltaTimes tpb notes

positions :: TicksPerBeat -> List MidiNote -> List (Tuple DeltaTime MidiNote)
positions tpb notes = flip zip notes $ sumDeltaTimes tpb notes

deltaTimes :: List MidiNote -> List DeltaTime
deltaTimes = map (\note -> note.deltaTime)

sumDeltaTimes :: TicksPerBeat -> List MidiNote -> List DeltaTime
sumDeltaTimes tpb notes = Cons 0.0 <<< reverse $ mapWithIndex (\_ i -> barPosition tpb <<< foldl (+) 0.0 $ take (length notes - i) $ deltaTimes notes) notes
      
-- TODO : TPB * Numerator ipv 4.0
-- TODO : Will cause rounding problems, because 'mod' does not accept Numbers...
barPosition :: TicksPerBeat -> DeltaTime -> DeltaTime
barPosition tpb d = toNumber <<< mod (round d) <<< round $ tpb * 4.0

-- Get index for notes that require beaming
--
eighthsIndex' :: TicksPerBeat -> List MidiNote -> List (Tuple Int Number)
eighthsIndex' ticksPerBeat = map (\(Tuple x (Tuple y z)) -> Tuple x y) <<< filter (\(Tuple _ (Tuple _ note)) -> note.deltaTime <= (ticksPerBeat / 2.0) * 1.5) <<< indexed
  where
    indexed :: List MidiNote -> List (Tuple Int (Tuple Number MidiNote))
    indexed notes = Data.List.Lazy.toUnfoldable <<< Data.List.Lazy.zip (Data.List.Lazy.iterate (_ + 1) 0) <<< Data.List.Lazy.fromFoldable $ positions ticksPerBeat notes

-- foldr
-- How to get access to remainder of list using fold?
beamsIndex :: Number -> List Int -> List (Tuple Int Number) -> List (List Int)
beamsIndex _ accXs Nil                               = Cons accXs          Nil
beamsIndex ticksPerBeat accXs (Cons x Nil)           = Cons (snoc accXs (fst x)) Nil
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | ((snd x) == ((ticksPerBeat / 4.0) * 2.0)) && ((snd y) == ((ticksPerBeat / 4.0) * 3.0)) = Cons (snoc (snoc accXs (fst x)) (fst y)) (beamsIndex ticksPerBeat Nil ys)
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | ((snd x) == ((ticksPerBeat / 4.0) * 6.0 )) && ((snd y) == ((ticksPerBeat / 4.0) * 8.0)) = Cons (snoc accXs (fst x)) (beamsIndex ticksPerBeat  Nil (Cons y ys))
beamsIndex ticksPerBeat accXs (Cons x (Cons y (Cons z zs))) | ((snd x) == ((ticksPerBeat / 4.0) * 2.0)) && ((snd z) == ((ticksPerBeat / 4.0) * 5.0)) = Cons (snoc accXs (fst x)) (beamsIndex ticksPerBeat  Nil (Cons y (Cons z zs)))
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | (snd x) == ((ticksPerBeat / 4.0) * 11.0) = Cons (snoc accXs (fst x)) (beamsIndex ticksPerBeat  Nil (Cons y ys))
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | (fst x) /= (fst y) - 1 = Cons accXs (beamsIndex ticksPerBeat  Nil (Cons y ys))
beamsIndex ticksPerBeat accXs (Cons x (Cons y ys)) | (fst x) == (fst y) - 1 = beamsIndex ticksPerBeat (snoc accXs (fst x)) (Cons y ys)

-- beamsIndex :: TicksPerBeat -> List (Tuple Int DeltaTime) -> List (List BeamIndex)
-- beamsIndex tpb notes = foldl fn {base : notes, indices: Nil} notes
--   where
--     fn b n = let nextNote       = fromMaybe (Tuple 0 480) $ index 1 b.base
--                  secondNextNote = fromMaybe (Tuple 0 480) $ index 2 b.base
--                  deltatime      = snd
--              in
--               if ((deltatime n) == ((ticksPerBeat / 4.0) * 2.0))  && ((deltatime nextNote) == ((ticksPerBeat / 4.0) * 3.0)) then
--                 { base  : slice 2 (length b.base - 1) b.base
--                 , notes : foldl (snoc b.indices <<< fst) $ drop 2 b.base }
--               else if ((deltatime n) == ((ticksPerBeat / 4.0) * 6.0 )) && ((deltatime y) == ((ticksPerBeat / 4.0) * 8.0))              ||
--                       ((deltatime n) == ((ticksPerBeat / 4.0) * 2.0))  && ((deltatime secondNextNote) == ((ticksPerBeat / 4.0) * 5.0)) ||
--                       (deltatime n) == ((ticksPerBeat / 4.0) * 11.0)
--                    then
--                      { base  : fromMaybe Nil $ tail b.base
--                      , notes : snoc b.indices $ fst n }
--                      else if 

-- TODO read about ST monad and arrays
midiEventWriter :: List MidiEvent -> List (Tuple MidiEvent Boolean)
midiEventWriter = map (\midiObject -> Tuple midiObject false)

--fold
duration :: List (Tuple MidiEvent Boolean) -> List MidiNote
duration Nil = Nil
duration midiEvents@(Cons (Tuple (NoteOn midiEvent) isRead) xs) =
  if midiEvent.subtype == "noteOn" then
    Cons { noteNumber   : midiEvent.noteNumber
         , deltaTime    : (sum midiEvent.noteNumber midiEvents) - midiEvent.deltaTime
         , hasFirstTie  : false
         , hasEndingTie : false
         , hasDot       : false }
    (duration (replaceBy (==) (noteOff) (toRead noteOff) xs))
  else duration xs
  where
    noteOff :: Tuple MidiEvent Boolean
    noteOff = fromRight $ findNoteOff midiEvent.noteNumber xs
    toRead :: Tuple MidiEvent Boolean -> Tuple MidiEvent Boolean
    toRead (Tuple midiEvent _) = Tuple midiEvent true
duration (Cons x xs) =
  duration xs

-- duration' :: List (Tuple MidiEvent Boolean) -> List MidiNote
-- duration' midiEvents = (_.midiNotes) $ foldl setDuration {base : events, notes : Nil} midiEvents 
--   where
--     setDuration b e = let noteOff :: Tuple MidiEvent Boolean
--                           -- TODO: Fix fromRight!!!!!
--                           noteOff = fromRight $ findNoteOff midiEvent.noteNumber xs
--                           toRead :: Tuple MidiEvent Boolean -> Tuple MidiEvent Boolean
--                           toRead (Tuple midiEvent _) = Tuple midiEvent true
--                           newNote =                        { noteNumber   : midiEvent.noteNumber
--                                                            , deltaTime    : (sum midiEvent.noteNumber midiEvents) - midiEvent.deltaTime
--                                                            , hasFirstTie  : false
--                                                            , hasEndingTie : false
--                                                            , hasDot       : false }
--                       in
--                        if true then
--                          { base  :
--                          , notes : }
--                            else
--                          { base  : tail b
--                          , notes : }
                       

sum :: NoteNumber -> List (Tuple MidiEvent Boolean) -> Number
sum n xs = foldl (\b x -> getDeltaTime x + b) 0.0 <<< flip take xs <<< (+) 1 <<< fromMaybe 0 $ findIndex (getNoteOff n) xs
    where
      getDeltaTime :: Tuple MidiEvent Boolean -> DeltaTime
      getDeltaTime (Tuple (NoteOn  x) _) = x.deltaTime
      getDeltaTime (Tuple (NoteOff x) _) = x.deltaTime

getNoteOff :: NoteNumber -> Tuple MidiEvent Boolean -> Boolean
getNoteOff n (Tuple (NoteOff note) isRead) | note.noteNumber == n && (not isRead) = true
getNoteOff _ _                                                                     = false

-- Error to console
findNoteOff' :: NoteNumber -> List (Tuple MidiEvent Boolean) -> Either String (Tuple MidiEvent Boolean)
findNoteOff' n = foldl (\b t -> if getNoteOff n t then Right t else b) (Left "No corresponding unread noteOff found.")

findNoteOff :: Int -> List (Tuple MidiEvent Boolean) -> Either String (Tuple MidiEvent Boolean)
findNoteOff _ Nil                                                  = Left "No corresponding unread noteOff found."
findNoteOff n (Cons noteOff@(Tuple (NoteOff midiEvent) isRead) xs) = if midiEvent.noteNumber == n && not isRead then Right noteOff else findNoteOff n xs
findNoteOff n (Cons _ xs)                                          = findNoteOff n xs

filterNotes :: MidiEvent -> Boolean
filterNotes (NoteOn  _) = true
filterNotes (NoteOff _) = true
filterNotes _           = false

filterNoteEvent :: Tuple MidiEvent Boolean -> Boolean
filterNoteEvent (Tuple (NoteOn  x) b) = true
filterNoteEvent (Tuple (NoteOff x) b) = true
filterNoteEvent _                     = false

getNumerator :: List MidiEvent -> Int
getNumerator = foldl (\b x -> num x b) 1
  where
    num x b = case x of
      (TimeSignature r) -> r.numerator
      _                 -> b 

getDeltaTimeNotes :: List MidiEvent -> List Number
getDeltaTimeNotes = map getDeltaTime <<< filter filterNotes
  where
    getDeltaTime (NoteOn  x) = x.deltaTime
    getDeltaTime (NoteOff x) = x.deltaTime

findStartingTies :: List MidiNote -> List Int
findStartingTies = catMaybes <<< mapWithIndex (\note i -> if note.hasFirstTie then pure i else Nothing)

midiNoteToVexFlowNote :: Number -> MidiNote -> VexFlowNote
midiNoteToVexFlowNote ticksPerBeat x = { pitch    : [vexNoteToVexFlowPitch $ midiNoteToVexTone x.noteNumber]
                                       , duration : deltaTimeToVexFlowDuration ticksPerBeat x.deltaTime}

midiNoteToVexNote :: TicksPerBeat -> MidiNote -> VexNote
midiNoteToVexNote tpb n = { note     : [midiNoteToVexTone n.noteNumber]
                          , duration : deltaTimeToVexFlowDuration tpb n.deltaTime}

midiNoteToVexTone :: Int -> VexTone
midiNoteToVexTone midiNote = { pitch      : fst <<< midiNoteToPartialVexFlowNote $ mod midiNote 12
                             , accidental : snd <<< midiNoteToPartialVexFlowNote $ mod midiNote 12
                             , octave     : (midiNoteToOctave midiNote) - 1}

replaceBy :: forall a. (a -> a -> Boolean) -> a -> a -> List a -> List a
replaceBy _ _ _ Nil                     = Nil
replaceBy (==) x z (Cons y ys) | x == y = Cons z ys
replaceBy (==) x z (Cons y ys)          = Cons y (replaceBy (==) x z ys)

toArray :: forall a. List (List a) -> Array (Array a)
toArray lst = toUnfoldable $ map (\x -> toUnfoldable x) lst

-- DISCUSS USING PARTIAL
fromRight :: forall a b. Either a b -> b
fromRight (Right a) = a
