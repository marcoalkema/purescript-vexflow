module VexFlow where

import Prelude
import Control.Monad.Eff


foreign import data VEXFLOW       :: !
foreign import data Canvas        :: *
foreign import data VexFlow       :: *                    
foreign import data DOM           :: !
type CanvasEff = Eff (dom         :: DOM) Canvas
type VexFlowEff = Eff (dom        :: DOM) VexFlow

type Clef = String
type Pitch = String
type Duration = String
type Note = { pitch               :: Pitch
             , duration           :: Duration
             }
type VexNote = {pitch             :: Array Pitch
                ,duration         :: Duration}
type Voice = Array VexNote
type Bar = Array Voice
type Octave = Number

foreign import createCanvas       :: String -> CanvasEff
foreign import createRenderer     :: Canvas -> VexFlowEff
foreign import createCtx          :: VexFlow -> VexFlowEff
foreign import createStave        :: Number -> Number -> Number -> VexFlowEff
-- Eff in DOM
foreign import drawStave          :: VexFlow -> Clef -> VexFlow -> VexFlowEff

foreign import createNotes        :: Bar -> VexFlowEff
foreign import createNewVoice     :: Number -> Number -> VexFlowEff
foreign import addNotesToVoice    :: VexFlow  -> VexFlowEff -> VexFlowEff
foreign import formatter          :: VexFlow -> Number -> VexFlowEff
foreign import drawVoice          :: VexFlow -> VexFlow -> VexFlow ->VexFlowEff

-- foreign import addNotesToVoice :: forall e. (List Note -> Octave -> Duration -> Eff (vexFlow :: VEXFLOW | e) Unit)
