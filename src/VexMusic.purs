module VexMusic where

import Prelude
import Data.Tuple
import Music

type VexAccidental = String
type VexPitch      = String
type VexOctave     = Int
type VexDuration   = Int

type VexFlowPitch      = String
type VexFlowDuration   = String
type VexFlowAccidental = String
type VexFlowVoice      = Array VexFlowNote
type VexFlowBar        = Array VexFlowVoice
type VexFlowMusic      = Array VexFlowBar
type VexFlowPiano      = { treble :: Array VexFlowBar
                         , bass   :: Array VexFlowBar
                         }

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

type VexVoice = Array VexNote
type VexBar = Array VexVoice
type VexMusic = Array VexBar

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

type VexAccidentalVoice = Array (Tuple Int String)
type VexAccidentalBar   = Array VexAccidentalVoice

noteToVexNote :: Note -> VexNote
noteToVexNote note_ = { note : [{ pitch      : note_.pitch
                                , accidental : note_.accidental
                                , octave     : note_.octave}]
                      , duration : note_.duration}

vexNoteToVexFlowPitch :: VexTone -> VexFlowPitch
vexNoteToVexFlowPitch vexTone = pitchToString vexTone.pitch ++ (accidentalToString vexTone.accidental) ++ "/" ++ show vexTone.octave

noteToVexFlowNote :: Note -> VexFlowNote
noteToVexFlowNote note = do
  let vexNote = noteToVexNote note
  vexNoteToVexFlowNote vexNote

vexNoteToVexFlowNote :: VexNote -> VexFlowNote
vexNoteToVexFlowNote vexNote = { pitch    : map vexNoteToVexFlowPitch vexNote.note
                               , duration : show $ durationToInt vexNote.duration}

vexNoteToVexFlowVoice :: VexVoice -> VexFlowVoice
vexNoteToVexFlowVoice = map vexNoteToVexFlowNote

vexNoteToVexFlowBar :: VexBar -> VexFlowBar
vexNoteToVexFlowBar = map vexNoteToVexFlowVoice

vexFlowCSharp :: VexFlowNote
vexFlowCSharp = noteToVexFlowNote cSharp

extractAccidentals :: VexNote -> Array Accidental
extractAccidentals = map _.accidental <<< _.note

addIndexToAccidentals :: Array Accidental -> Array (Tuple Accidental Int)
addIndexToAccidentals arr = Data.List.Lazy.toUnfoldable $ (Data.List.Lazy.zip (Data.List.Lazy.fromFoldable arr) (Data.List.Lazy.iterate (+1) 0))

isNatural :: (Tuple Accidental Int) -> Boolean
isNatural (Tuple a b) = a /= Natural

filterAccidentals :: Array (Tuple Accidental Int) -> Array (Tuple Accidental Int)
filterAccidentals = Data.Array.filter isNatural

accidentalToVexFlowAccidental :: Tuple Accidental Int -> Tuple VexFlowAccidental Int
accidentalToVexFlowAccidental (Tuple accidental i) = Tuple (accidentalToString accidental) i

vexnoteToIndexedAccidentals :: VexNote -> Array (Tuple VexFlowAccidental Int)
vexnoteToIndexedAccidentals vexFlowNote = do
  let accidentals = extractAccidentals vexFlowNote
  let indexedAccidentals = addIndexToAccidentals accidentals
  let filteredAccidentals = filterAccidentals indexedAccidentals
  map accidentalToVexFlowAccidental filteredAccidentals

vexVoiceToIndexedAccidentals :: VexVoice -> Array (Array (Tuple VexFlowAccidental Int))
vexVoiceToIndexedAccidentals = map vexnoteToIndexedAccidentals

vexBarToIndexedAccidentals :: VexBar -> Array (Array (Array (Tuple VexFlowAccidental Int)))
vexBarToIndexedAccidentals = map vexVoiceToIndexedAccidentals

eighth :: VexNote
eighth = { note : [ { pitch : A
                    , accidental : Flat
                    , octave : 4}
                  , { pitch : C
                    , accidental : Natural
                    , octave : 5}
                  , { pitch : E
                    , accidental : Natural
                    , octave : 5}
                  ]
         , duration : Even Eighth}

eighthsMusic :: VexMusic
eighthsMusic = [ [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               , [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               , [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               , [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               , [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               , [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               , [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               , [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               , [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]
               ]
musicWithIndexedAccidentals :: VexMusic -> Array (Array (Array (Array (Tuple VexFlowAccidental Int))))
musicWithIndexedAccidentals = map vexBarToIndexedAccidentals

testMusic :: VexMusic -> VexFlowMusic
testMusic = map vexNoteToVexFlowBar
  
  
