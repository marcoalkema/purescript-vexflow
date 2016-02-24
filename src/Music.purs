module Music where

data Pitch      = C | D | E | F | G | A | B
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
data Duration   = Regular | Irregular
data Regular    = SixtyFourth | ThirtySecond | Sixteenth | Eighth | Quarter | Half | Whole
data Irregular  = Tuplet

-- type Note = { pitch      :: Pitch
--             , accidental :: Accidental
--             , duration   :: Duration
--             }
