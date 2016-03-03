module VexFlow where

import Prelude
import Control.Monad.Eff
import Music
import VexMusic
import Data.Tuple


foreign import data VEXFLOW       :: !
foreign import data Canvas        :: *
foreign import data VexFlow       :: *                    
foreign import data DOM           :: !
type CanvasEff  = Eff (dom :: DOM) Canvas
type VexFlowEff = Eff (dom :: DOM) VexFlow

foreign import createCanvas       :: String -> CanvasEff
foreign import createRenderer     :: Canvas -> VexFlowEff
foreign import createCtx          :: VexFlow -> VexFlowEff
foreign import createStave        :: Number -> Number -> Number -> VexFlowEff
-- Eff in DOM
foreign import drawStave          :: VexFlow -> VexFlow -> VexFlowEff
foreign import drawKeyStave       :: VexFlow -> Clef -> VexFlow -> VexFlowEff
foreign import createKeySignature :: KeySignature -> VexFlow -> VexFlowEff
foreign import createTimeSignature :: TimeSignature -> VexFlow -> VexFlowEff
foreign import createNotes        :: VexFlowBar -> VexFlowEff
foreign import addAccidentals     :: VexFlow -> Array (Array (Array (Tuple String Int))) -> VexFlowEff
foreign import addBeams           :: VexFlow -> VexFlowEff
foreign import createNewVoice     :: Number  -> Number -> VexFlowEff
foreign import addNotesToVoice    :: VexFlow -> VexFlowEff -> VexFlowEff
foreign import formatter          :: VexFlow -> Number -> VexFlowEff
foreign import drawVoice          :: VexFlow -> VexFlow -> VexFlow ->VexFlowEff
foreign import drawBeams          :: VexFlow -> VexFlow -> VexFlowEff
foreign import drawTies           :: VexFlow -> VexFlowEff

foreign import logger             :: forall a. a -> VexFlowEff
