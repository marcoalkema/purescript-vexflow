module Main where

-- import Prelude (Unit, (+), (-), ($), bind, (*), const, show, map, (==), (||), (<), (&&))
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console
import Data.Tuple (Tuple(Tuple), fst, snd)
import VexFlow
import VexMusic
import Music (KeySignature, Clef)
import MidiJS as MidiPlayer
import MidiJsTypes
import Data.Foreign (Foreign, unsafeFromForeign, toForeign, typeOf)
import Data.Foldable (foldl)
import Data.List (List(Nil, Cons), snoc, toUnfoldable, toList, delete)
import Data.Either
import MidiToVexFlow
import Data.Array
import Data.Tuple
import Data.Maybe
--y
foreign import data CONSOLE :: !

main :: forall e. Eff (dom :: DOM, midi :: MidiPlayer.MIDI, vexFlow :: VEXFLOW | e) Unit
main = do
  MidiPlayer.loadFile "bower_components/purescript-midiplayer/midi/ties4.mid"
  MidiPlayer.loadPlugin { soundfontUrl: "bower_components/midi/examples/soundfont/"
                  , instrument:   "acoustic_grand_piano"
                  }
    (const (parseMidi MidiPlayer.getData3))
