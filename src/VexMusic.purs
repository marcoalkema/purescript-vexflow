module VexMusic where

import Prelude
import Data.Tuple
import Music

type VexAccidental = String
type VexPitch      = String
type VexOctave     = Int
type VexDuration   = Int

type VexFlowPitch    = String
type VexFlowDuration = String
type VexFlowVoice    = Array VexFlowNote
type VexFlowBar      = Array VexFlowVoice

type VexNote = { note     :: Array VexTone
                   , duration :: Duration
                   }

type VexTone = { pitch      :: Pitch
               , accidental :: Accidental
               , octave     :: Octave
               }

type VexFlowNote = { pitch     :: Array VexFlowPitch
                   , duration  :: VexFlowDuration
                   }

pitchToString :: Pitch -> VexPitch
pitchToString C = "c"
pitchToString D = "d"
pitchToString E = "e"
pitchToString F = "f"
pitchToString G = "g"
pitchToString A = "a"
pitchToString B = "b"

accidentalToString :: Accidental -> VexAccidental
accidentalToString DoubleFlat  = "bb"
accidentalToString Flat        = "b"
accidentalToString Natural     = ""
accidentalToString Sharp       = "#"
accidentalToString DoubleSharp = "##"

durationToInt :: Duration -> VexDuration
durationToInt (Even SixtyFourth)  = 64
durationToInt (Even ThirtySecond) = 32
durationToInt (Even Sixteenth)    = 16
durationToInt (Even Eighth)       = 8
durationToInt (Even Quarter)      = 4
durationToInt (Even Half)         = 2
durationToInt (Even Whole)        = 1
durationToInt (Tuplet Three)      = 3
durationToInt (Tuplet Five)       = 5
durationToInt (Tuplet Six)        = 6
durationToInt (Tuplet Seven)      = 7

noteToVexFlowPitch :: Pitch -> Accidental -> Octave -> VexFlowPitch
noteToVexFlowPitch pitch accidental octave = (pitchToString pitch) ++ (accidentalToString accidental) ++ "/" ++ show octave

noteToVexFlowNote :: Note -> VexFlowNote
noteToVexFlowNote note = {pitch    : [noteToVexFlowPitch note.pitch note.accidental note.octave]
                         ,duration : show $ durationToInt note.duration
                         }                     

vexFlowCis :: VexFlowNote
vexFlowCis = noteToVexFlowNote cSharp

cIS :: VexNote
cIS = { note     : cISNote
      , duration : Even Whole
      }
              
cISNote :: Array VexTone
cISNote = [{ pitch      : C
           , accidental : Sharp
           , octave     : 5}
          , { pitch      : E
            , accidental : Natural
            , octave     : 5}
          , { pitch      : G
            , accidental : Flat
            , octave     : 5
            }]
          
indexedCis :: Array (Tuple Int Accidental)
indexedCis = vexnoteToIndexedAccidentals cIS

extractAccidentals :: VexNote -> Array Accidental
extractAccidentals = map _.accidental <<< _.note

addIndexToAccidentals :: Array Accidental -> Array (Tuple Int Accidental)
addIndexToAccidentals = Data.Array.zip $ Data.List.Lazy.toUnfoldable $ (Data.List.Lazy.iterate (+1) 0)

isNatural :: (Tuple Int Accidental) -> Boolean
isNatural (Tuple a b) = b /= Natural

filterAccidentals :: Array (Tuple Int Accidental) -> Array (Tuple Int Accidental)
filterAccidentals = Data.Array.filter isNatural

vexnoteToIndexedAccidentals :: VexNote -> Array (Tuple Int Accidental)
vexnoteToIndexedAccidentals vexFlowNote = do
  let accidentals = extractAccidentals vexFlowNote
  let indexedAccidentals = addIndexToAccidentals accidentals
  filterAccidentals indexedAccidentals

