module Music where

import VexFlow as Vx
import Prelude
import Data.List
import Data.Tuple
import Data.Generic

data Pitch      = C | D | E | F | G | A | B

data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
derive instance genericAccidental :: Generic Accidental
instance eqAccidental :: Eq Accidental where
  eq = gEq

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

type VexFlowNote = { note     :: Data.List.Lazy.List VexNote
                , duration :: Duration
                }
              
type VexPitch = String

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
                     
accidentalIndex :: Note -> List Int
accidentalIndex note = toList [durationToInt note.duration]
                     
vexCis :: Vx.VexNote
vexCis = noteToVexNote cis
-- noteToVexNote note = {pitch: ["b/5"], duration: "h"}


cIS :: VexFlowNote
cIS = { note     : cISNote
      , duration : Even Whole
      }
              
cISNote :: Data.List.Lazy.List VexNote
cISNote = Data.List.Lazy.toList [ { pitch      : C
                                 , accidental : Sharp
                                 , octave     : 5}
                               , { pitch      : E
                                 , accidental : Natural
                                 , octave     : 5}
                               , { pitch      : G
                                 , accidental : Sharp
                                 , octave     : 5
                                 }]

extractAccidentals :: VexFlowNote -> Data.List.Lazy.List Accidental
extractAccidentals vexFlowNote = map (\vexNote -> vexNote.accidental) vexFlowNote.note

addIndexToAccidentals :: Data.List.Lazy.List Accidental -> Data.List.Lazy.List (Tuple Int Accidental)
addIndexToAccidentals accidentals = Data.List.Lazy.zip (Data.List.Lazy.iterate (+1) 0) accidentals

isNatural :: (Tuple Int Accidental) -> Boolean
isNatural (Tuple a b) = if b /= Natural then
                             true
                           else
                             false

filterAccidentals :: Data.List.Lazy.List (Tuple Int Accidental) -> Data.List.Lazy.List (Tuple Int Accidental)
filterAccidentals vexNotesIndex = Data.List.Lazy.filter isNatural vexNotesIndex


vexnoteToIndexedAccidentals :: VexFlowNote -> Data.List.Lazy.List (Tuple Int Accidental)
vexnoteToIndexedAccidentals vexFlowNote = filterAccidentals $ addIndexToAccidentals $ extractAccidentals vexFlowNote

-- vexnoteToIndexedAccidentals2 :: VexFlowNote -> Data.List.Lazy.List (Tuple Int Accidental)
-- vexnoteToIndexedAccidentals2 vexFlowNote = do
--   accidentals <- extractAccidentals vexFlowNote
--   indexedAccidentals <- addIndexToAccidentals accidentals
--   filterAccidentals indexedAccidentals
