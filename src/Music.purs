module Music where

import VexFlow as Vx
import Prelude
import Data.List.Lazy
import Data.Tuple
import Data.Generic

data Pitch      = C | D | E | F | G | A | B

data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
derive instance genericAccidental :: Generic Accidental
instance eqAccidental :: Eq Accidental where
  eq = gEq
instance showAccidental :: Show Accidental where
  show = gShow 

type Octave     = Int
data Duration   = Even Regular| Tuplet Irregular 
data Regular    = SixtyFourth | ThirtySecond | Sixteenth | Eighth | Quarter | Half | Whole
data Irregular  = Three | Five | Six | Seven

type Note = { pitch      :: Pitch
            , accidental :: Accidental
            , octave     :: Int
            , duration   :: Duration  
            }
            
type VexNote = { pitch      :: Pitch
             , accidental :: Accidental
             , octave     :: Int
             }

type VexFlowNote = { note     :: Array VexNote
                , duration :: Duration
                }
              
type VexPitch = String
type VexAccidental = String

cis :: Note
cis = { pitch      : C
      , accidental : Sharp
      , octave     : 5
      , duration   : Even Whole
      }

pitchToString :: Pitch -> String
pitchToString C = "c"
pitchToString D = "d"
pitchToString E = "e"
pitchToString F = "f"
pitchToString G = "g"
pitchToString A = "a"
pitchToString B = "b"

accidentalToString :: Accidental -> String
accidentalToString DoubleFlat  = "bb"
accidentalToString Flat        = "b"
accidentalToString Natural     = ""
accidentalToString Sharp       = "#"
accidentalToString DoubleSharp = "##"

durationToInt :: Duration -> Int
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

pitchToVexPitch :: Pitch -> Accidental -> Octave -> VexPitch
pitchToVexPitch pitch accidental octave = (pitchToString pitch) ++ (accidentalToString accidental) ++ "/" ++ show octave

noteToVexNote :: Note -> Vx.VexNote
noteToVexNote note = {pitch    : [pitchToVexPitch note.pitch note.accidental note.octave]
                     ,duration : show $ durationToInt note.duration
                     }                     
                     
vexCis :: Vx.VexNote
vexCis = noteToVexNote cis
-- noteToVexNote note = {pitch: ["b/5"], duration: "h"}


cIS :: VexFlowNote
cIS = { note     : cISNote
      , duration : Even Whole
      }
              
cISNote :: Array VexNote
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

extractAccidentals :: VexFlowNote -> Array Accidental
extractAccidentals = map _.accidental <<< _.note

addIndexToAccidentals :: Array Accidental -> Array (Tuple Int Accidental)
addIndexToAccidentals = Data.Array.zip $ Data.List.Lazy.toUnfoldable $ (Data.List.Lazy.iterate (+1) 0)

isNatural :: (Tuple Int Accidental) -> Boolean
isNatural (Tuple a b) = b /= Natural

filterAccidentals :: Array (Tuple Int Accidental) -> Array (Tuple Int Accidental)
filterAccidentals = Data.Array.filter isNatural

vexnoteToIndexedAccidentals :: VexFlowNote -> Array (Tuple Int Accidental)
vexnoteToIndexedAccidentals vexFlowNote = do
  let accidentals = extractAccidentals vexFlowNote
  let indexedAccidentals = addIndexToAccidentals accidentals
  filterAccidentals indexedAccidentals
