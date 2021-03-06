module VexMusic where

import Prelude
import Data.Tuple
import Data.Array (filter)
import Music

type MidiNote      = Int

type VexAccidental = String
type VexPitch      = String
type VexOctave     = Int
type VexDuration   = Int

type VexFlowPitch      = String
type VexFlowDuration   = String
type VexFlowAccidental = String
type VexFlowOctave     = String
type VexFlowVoice      = Array VexFlowNote
type VexFlowBar        = Array VexFlowVoice
type VexFlowMusic      = Array VexFlowBar

type VexFlowAccidentalNote  = Array (Tuple VexFlowAccidental Int)
type VexFlowAccidentalVoice = Array VexFlowAccidentalNote
type VexFlowAccidentalBar   = Array VexFlowAccidentalVoice
type VexFlowAccidentalMusic = Array VexFlowAccidentalBar
                         
type VexNote     = { note     :: Array VexTone
                   , duration :: VexFlowDuration
                   }

type VexTone     = { pitch      :: Pitch
                   , accidental :: Accidental
                   , octave     :: Octave
                   }

type VexFlowNote = { pitch     :: Array VexFlowPitch
                   , duration  :: VexFlowDuration
                   }

type VexVoice = Array VexNote
type VexBar   = Array VexVoice
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

midiNoteToOctave :: Int -> Octave
midiNoteToOctave n = (n - (mod n 12)) / 12

deltaTimeToVexDuration :: Number -> Number -> Int
deltaTimeToVexDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat / 4.0 = 16
deltaTimeToVexDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat / 2.0 = 8
deltaTimeToVexDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat       = 4
deltaTimeToVexDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat * 2.0 = 2
deltaTimeToVexDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat * 3.0 = 1
deltaTimeToVexDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat * 4.0 = 1

deltaTimeToVexFlowDuration :: Number -> Number -> VexFlowDuration
deltaTimeToVexFlowDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat / 4.0         = show 16
deltaTimeToVexFlowDuration ticksPerBeat deltaTime | deltaTime == (ticksPerBeat / 2.0) * 1.5 = show 8 ++ "d"
deltaTimeToVexFlowDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat / 2.0         = show 8
deltaTimeToVexFlowDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat               = show 4
deltaTimeToVexFlowDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat * 1.5         = show 4 ++ "d"
deltaTimeToVexFlowDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat * 2.0         = show 2
deltaTimeToVexFlowDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat * 3.0         = show 1
deltaTimeToVexFlowDuration ticksPerBeat deltaTime | deltaTime == ticksPerBeat * 4.0         = show 1

midiNoteToPartialVexFlowNote :: Int -> (Tuple Pitch Accidental)
midiNoteToPartialVexFlowNote 0  = Tuple C Natural
midiNoteToPartialVexFlowNote 1  = Tuple C Sharp
midiNoteToPartialVexFlowNote 2  = Tuple D Natural
midiNoteToPartialVexFlowNote 3  = Tuple D Sharp
midiNoteToPartialVexFlowNote 4  = Tuple E Natural
midiNoteToPartialVexFlowNote 5  = Tuple F Natural
midiNoteToPartialVexFlowNote 6  = Tuple F Sharp
midiNoteToPartialVexFlowNote 7  = Tuple G Natural
midiNoteToPartialVexFlowNote 8  = Tuple G Sharp
midiNoteToPartialVexFlowNote 9  = Tuple A Natural
midiNoteToPartialVexFlowNote 10 = Tuple A Sharp
midiNoteToPartialVexFlowNote 11 = Tuple B Natural

noteToVexNote :: Note -> VexNote
noteToVexNote note' = { note : [{ pitch      : note'.pitch
                                , accidental : note'.accidental
                                , octave     : note'.octave }]
                      , duration : show $ durationToInt note'.duration}

noteToVexFlowNote :: Note -> VexFlowNote
noteToVexFlowNote note = do
  let vexNote = noteToVexNote note
  vexNoteToVexFlowNote vexNote

vexNoteToVexFlowPitch :: VexTone -> VexFlowPitch
vexNoteToVexFlowPitch vexTone = pitchToString vexTone.pitch ++ (accidentalToString vexTone.accidental) ++ "/" ++ show vexTone.octave

vexNoteToVexFlowNote :: VexNote -> VexFlowNote
vexNoteToVexFlowNote vexNote = { pitch    : map vexNoteToVexFlowPitch vexNote.note
                               , duration : vexNote.duration}
vexNoteToVexFlowVoice :: VexVoice -> VexFlowVoice
vexNoteToVexFlowVoice = map vexNoteToVexFlowNote

vexNoteToVexFlowBar :: VexBar -> VexFlowBar
vexNoteToVexFlowBar = map vexNoteToVexFlowVoice

extractAccidentals :: VexNote -> Array Accidental
extractAccidentals = map _.accidental <<< _.note

addIndexToAccidentals :: Array Accidental -> Array (Tuple Accidental Int)
addIndexToAccidentals arr = Data.List.Lazy.toUnfoldable $ (Data.List.Lazy.zip (Data.List.Lazy.fromFoldable arr) (Data.List.Lazy.iterate (_ + 1) 0))

isNatural :: (Tuple Accidental Int) -> Boolean
isNatural (Tuple a b) = a /= Natural

filterAccidentals :: Array (Tuple Accidental Int) -> Array (Tuple Accidental Int)
filterAccidentals = filter isNatural

accidentalToVexFlowAccidental :: Tuple Accidental Int -> Tuple VexFlowAccidental Int
accidentalToVexFlowAccidental (Tuple accidental i) = Tuple (accidentalToString accidental) i

vexnoteToIndexedAccidentals :: VexNote -> VexFlowAccidentalNote
vexnoteToIndexedAccidentals vexFlowNote = do
  let accidentals         = extractAccidentals vexFlowNote
      indexedAccidentals  = addIndexToAccidentals accidentals
      filteredAccidentals = filterAccidentals indexedAccidentals
  map accidentalToVexFlowAccidental filteredAccidentals

vexVoiceToIndexedAccidentals :: VexVoice -> VexFlowAccidentalVoice
vexVoiceToIndexedAccidentals = map vexnoteToIndexedAccidentals

vexBarToIndexedAccidentals :: VexBar -> VexFlowAccidentalBar
vexBarToIndexedAccidentals = map vexVoiceToIndexedAccidentals

musicWithIndexedAccidentals :: VexMusic -> VexFlowAccidentalMusic
musicWithIndexedAccidentals = map vexBarToIndexedAccidentals

  
