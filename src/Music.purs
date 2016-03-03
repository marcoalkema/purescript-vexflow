module Music where

import Prelude
import Data.Generic

data Pitch      = C | D | E | F | G | A | B
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
type Octave     = Int
data Duration   = Even Regular| Tuplet Irregular 
data Regular    = SixtyFourth | ThirtySecond | Sixteenth | Eighth | Quarter | Half | Whole
data Irregular  = Three | Five | Six | Seven

type Clef = String
type KeySignature = String
type TimeSignature = String


type Note = { pitch      :: Pitch
            , accidental :: Accidental
            , octave     :: Int
            , duration   :: Duration  
            }

type ChordNote = { pitch      :: Pitch
                 , accidental :: Accidental
                 , octave     :: Int
                 }

type Chord = { notes      :: Array ChordNote
             , duration   :: Duration  
             }

cSharp :: Note
cSharp = { pitch      : C
      , accidental : Sharp
      , octave     : 5
      , duration   : Even Whole
      }


derive instance genericAccidental :: Generic Accidental
instance eqAccidental :: Eq Accidental where
  eq = gEq
instance showAccidental :: Show Accidental where
  show = gShow 


                   

