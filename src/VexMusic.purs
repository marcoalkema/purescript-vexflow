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
vexNoteToVexFlowVoice vexVoice = map vexNoteToVexFlowNote vexVoice

vexNoteToVexFlowBar :: VexBar -> VexFlowBar
vexNoteToVexFlowBar vexBar = map vexNoteToVexFlowVoice vexBar

vexFlowCSharp :: VexFlowNote
vexFlowCSharp = noteToVexFlowNote cSharp

extractAccidentals :: VexNote -> Array Accidental
extractAccidentals = map _.accidental <<< _.note

addIndexToAccidentals :: Array Accidental -> Array (Tuple Accidental Int)
addIndexToAccidentals arr = Data.List.Lazy.toUnfoldable $ (Data.List.Lazy.zip (Data.List.Lazy.fromFoldable arr) (Data.List.Lazy.iterate (+1) 0))

isNatural :: (Tuple Accidental Int) -> Boolean
isNatural (Tuple a b) = a /= Natural

-- isNatural2 :: Array (Tuple Accidental Int) -> Array (Tuple Accidental Int)
-- isNatural2 

-- removeElement :: 

-- accidentalList :: Array (Tuple Accidental Int) -> Array (Tuple Accidental Int)
-- accidentalList 

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
vexVoiceToIndexedAccidentals voice = map vexnoteToIndexedAccidentals voice

vexBarToIndexedAccidentals :: VexBar -> Array (Array (Array (Tuple VexFlowAccidental Int)))
vexBarToIndexedAccidentals bar = map vexVoiceToIndexedAccidentals bar

cIS :: VexNote
cIS = { note     : cISNote
      , duration : Even Half
      }

indexCIS :: Array (Tuple VexFlowAccidental Int)
indexCIS = vexnoteToIndexedAccidentals cIS
      
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

aIS :: VexNote
aIS = { note : aISNote
      , duration : Even Half
      }

aISNote :: Array VexTone
aISNote = [{ pitch      : A
           , accidental : Sharp
           , octave     : 5}
          , { pitch      : C
            , accidental : Sharp
            , octave     : 5}
          , { pitch      : E
            , accidental : Sharp
            , octave     : 5
            }]

kip :: VexFlowNote
kip = vexNoteToVexFlowNote cIS

testBar :: VexFlowVoice
testBar = map vexNoteToVexFlowNote [cIS, aIS]

eighth :: VexNote
eighth = { note : [{ pitch : A
                   , accidental : Sharp
                   , octave : 4}
                  , { pitch : C
                    , accidental : Sharp
                    , octave : 4}
                  , { pitch : E
                    , accidental : Natural
                    , octave : 4}
                  ]
         , duration : Even Eighth}

eighths :: VexVoice
eighths = [eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]
         
eighthsBar :: VexBar
eighthsBar = [[eighth, eighth, eighth, eighth, eighth, eighth, eighth, eighth]]

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
aapTuple :: VexMusic -> Array (Array (Array (Array (Tuple VexFlowAccidental Int))))
aapTuple vex = map vexBarToIndexedAccidentals vex

aapMuzak :: VexMusic -> VexFlowMusic
aapMuzak muzakje = map vexNoteToVexFlowBar muzakje
  
  
