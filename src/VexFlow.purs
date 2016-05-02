module VexFlow where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console
import Music (TimeSignature, KeySignature, Clef)
import VexMusic
import Data.Tuple (Tuple)
import Data.Maybe
import Data.Tuple
import MidiPlayer
import Data.List
import Data.Int (toNumber)
import Data.Array (zipWith, index)
import Data.Foldable (traverse_)

foreign import data VEXFLOW :: !
foreign import data VexFlow :: *
foreign import data CANVAS  :: !
foreign import data Canvas  :: *

foreign import createCanvas        :: forall e. String        -> Eff (vexFlow :: VEXFLOW | e) Canvas
foreign import createRenderer      :: forall e. Canvas        -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createCtx           :: forall e. VexFlow       -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createNotes         :: forall e. VexFlowBar    -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createStave         :: forall e. Int           -> Int       -> Number  -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import drawKeyStave        :: forall e. VexFlow       -> Clef      -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawVoice           :: forall e. VexFlow       -> VexFlow   -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import formatter           :: forall e. VexFlow       -> Number                             -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import createNewVoice      :: forall e. Int           -> Number                             -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addAccidentals      :: forall e. VexFlow       -> AccidentalBar                      -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import drawStave           :: forall e. VexFlow       -> VexFlow                            -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import createKeySignature  :: forall e. KeySignature  -> VexFlow                            -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createTimeSignature :: forall e. TimeSignature -> VexFlow                            -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addTies             :: forall e. VexFlow       -> Array TieIndex                     -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addBeams            :: forall e. VexFlow       -> Array (Array BeamIndex)            -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addNotesToVoice     :: forall e. VexFlow       -> (Eff (vexFlow :: VEXFLOW) VexFlow) -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import drawBeams           :: forall e. VexFlow       -> VexFlow                            -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawTies            :: forall e. VexFlow       -> VexFlow                            -> Eff (vexFlow :: VEXFLOW | e) Unit

type AccidentalBar   = (Array (Array (Array (Tuple String Int))))
type AccidentalVoice = Array AccidentalBar
type TieIndex        = Int
type BeamIndex       = Int

renderNotation :: forall e. Canvas -> VexFlowMusic -> VexMusic -> Array (Array TieIndex) -> Array (Array (Array BeamIndex)) -> Int -> Eff (vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI | e) Unit
renderNotation canvas notes vexNotes indexedTies indexedBeams num = do
  renderer <- createRenderer canvas
  drawPrimaryStave renderer "treble" "C" num
  drawNotation num notes (musicWithIndexedAccidentals vexNotes) renderer indexedTies indexedBeams

drawNotation :: forall e. Int -> VexFlowMusic -> AccidentalVoice -> VexFlow -> Array (Array TieIndex) -> Array (Array (Array BeamIndex)) -> Eff (vexFlow :: VEXFLOW | e) Unit
drawNotation num music accidentals renderer indexedTies indexedBeams = do
  let stave         = renderStaff renderer 280.0
      voices        = zipWith (renderVoice num) music accidentals
      indexedVoices = addIndexToArray voices
  traverse_ (\(Tuple i voice) -> stave i $ voice (fromJust $ index indexedTies i) (fromJust $ index indexedBeams i)) indexedVoices

-- createNewVoice with denominator & numerator
renderVoice :: forall e. Int -> VexFlowBar -> AccidentalBar -> Array TieIndex -> Array (Array BeamIndex) -> VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
renderVoice num bar accidentals indexedTies indexedBeams context stave = do
  notes            <- createNotes bar
  addedAccidentals <- addAccidentals notes accidentals
  tiedNotes        <- addTies addedAccidentals indexedTies
  beamedNotes      <- addBeams addedAccidentals indexedBeams
  voicing          <- addNotesToVoice addedAccidentals (createNewVoice num 4.0)
  formatter voicing (260.0)
  drawVoice context stave voicing
  drawTies tiedNotes context
  drawBeams beamedNotes context

-- createStaff: Break system per 4 bars
renderStaff :: forall e. VexFlow -> Number -> Int -> (VexFlow -> VexFlow -> (Eff (vexFlow :: VEXFLOW | e) Unit)) -> Eff (vexFlow :: VEXFLOW | e) Unit
renderStaff renderer w i voice = do
  ctx   <- createCtx renderer
  stave <- createStave (80 + (mod i 4) * 280) (0 + (50 * (i - (mod i 4)))) w
  drawStave stave ctx
  voice ctx stave

-- createTimeSignature with numerator
drawPrimaryStave :: forall e. VexFlow -> Clef -> KeySignature -> Int -> Eff (vexFlow :: VEXFLOW | e) Unit
drawPrimaryStave renderer clef key num = do
    ctx   <- createCtx renderer
    stave <- createStave 1 1 80.0
    createKeySignature key stave
    createTimeSignature ((show num) ++ "/4") stave
    drawKeyStave stave clef ctx

-- drawTrebleStave :: Number -> Vx.VexFlow -> KeySignature -> Vx.VexFlowEff
-- drawTrebleStave y renderer key = do
--     ctx <- Vx.createCtx renderer
--     stave <- Vx.createStave 1.0 1 80.0
--     Vx.createKeySignature key stave
--     Vx.createTimeSignature "4/4" stave
--     Vx.drawKeyStave stave "treble" ctx

-- drawBassStave :: Number -> Vx.VexFlow -> KeySignature -> Vx.VexFlowEff
-- drawBassStave y renderer key = do
--     ctx <- Vx.createCtx renderer
--     stave <- Vx.createStave 1.0 y 80.0
--     Vx.createKeySignature key stave
--     Vx.createTimeSignature "4/4" stave
--     Vx.drawKeyStave stave "bass" ctx

fromJust :: forall a. Maybe a -> a
fromJust (Just x) = x

mapWithIndex :: forall a b. (a -> Int -> b) -> List a -> List b
mapWithIndex f lst = Data.List.reverse $ go 0 lst Nil
  where
  go _ Nil acc = acc
  go n (Cons x xs) acc = go (n + 1) xs $ Cons (f x n) acc

addIndexToArray :: forall a. Array a  -> Array (Tuple Int a)
addIndexToArray arr = Data.List.Lazy.toUnfoldable $ Data.List.Lazy.zip (Data.List.Lazy.iterate (_ + 1) 0) (Data.List.Lazy.fromFoldable arr)
