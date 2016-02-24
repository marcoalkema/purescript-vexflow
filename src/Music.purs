module Music where

import VexFlow as Vx
import Prelude
import Data.List

data Pitch      = C | D | E | F | G | A | B
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
type Octave     = Int
data Duration   = Even Regular| Tuplet Irregular 
data Regular    = SixtyFourth | ThirtySecond | Sixteenth | Eighth | Quarter | Half | Whole
data Irregular  = Three | Five | Six | Seven

type Note = { pitch      :: Pitch
            , accidental :: Accidental
            , octave     :: Int
            , duration   :: Duration  
            }
            
type Note_ = { pitch      :: Pitch
             , accidental :: Accidental
             , octave     :: Int
             }

type VexNote_ = { note     :: Array Note
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
                     
aap :: Vx.VexNote
aap = noteToVexNote cis
-- noteToVexNote note = {pitch: ["b/5"], duration: "h"}
