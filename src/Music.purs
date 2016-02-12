module Music where

data Pitch = C | D | E | F | G | A | B
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
type Duration = SixtyFourth | ThirtySecond | Sixteenth | Eighth | Quarter | Half | Whole


type Note = { pitch      :: Pitch
            , accidental :: Accidental
            , duration   :: Duration
            }
