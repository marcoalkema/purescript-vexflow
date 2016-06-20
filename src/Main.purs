module Vex where

import Prelude (Unit, (>>=), const, bind)
import Control.Monad.Eff (Eff)
import VexFlow (VEXFLOW, createCanvas)
import MidiJS as MidiPlayer
import MidiToVexFlow (renderMidi)
import Signal


--TODO: Make test-example independent from MidiPlayer

main :: forall e. Eff (midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW | e) Unit
main = do
  canvas <- createCanvas "notationCanvas"
  MidiPlayer.loadFile "bower_components/purescript-midiplayer/midi/test4.mid"
  -- MidiPlayer.loadPlugin { soundfontUrl: "bower_components/midi/examples/soundfont/"
  --                       , instrument:   "acoustic_grand_piano"
  --                       }
  --   (const (MidiPlayer.getData >>= renderMidi canvas))

