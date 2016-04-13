module Main where

import Prelude
import Control.Monad.Eff (Eff)
import VexFlow
import MidiJS as MidiPlayer
import MidiToVexFlow

main :: forall e. Eff (midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW | e) Unit
main = do
  MidiPlayer.loadFile "bower_components/purescript-midiplayer/midi/bach10.mid"
  MidiPlayer.loadPlugin { soundfontUrl: "bower_components/midi/examples/soundfont/"
                        , instrument:   "acoustic_grand_piano"
                        }
    (const (parseMidi MidiPlayer.getData3))
