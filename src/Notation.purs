module Notation where

data Clef          = Treble | Bass | Alto | Tenor
type TimeSignature = {numerator :: Int, denominator :: Int}
type KeySignature  = String
type Tempo         = Int
