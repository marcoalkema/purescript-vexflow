module Main where

import Prelude
import Control.Monad.Eff (Eff)
import VexFlow
import MidiJS as MidiPlayer
import MidiToVexFlow

main :: forall e. Eff (midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW | e) Unit
main = do
  -- canvas <- createCanvas "notationCanvas"
  MidiPlayer.loadFile "bower_components/purescript-midiplayer/midi/test2.mid"
  MidiPlayer.loadPlugin { soundfontUrl: "bower_components/midi/examples/soundfont/"
                        , instrument:   "acoustic_grand_piano"
                        }
    (const (MidiPlayer.getData >>= parseMidi))
