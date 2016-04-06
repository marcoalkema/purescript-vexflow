module VexFlow where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Music (TimeSignature, KeySignature, Clef)
import VexMusic (VexFlowBar)
import Data.Tuple (Tuple)

foreign import data VEXFLOW       :: !
foreign import data VexFlow       :: *
foreign import data CANVAS        :: !
foreign import data Canvas        :: *                    
foreign import data DOM           :: !
foreign import data Dom           :: *                    

foreign import clef               :: String
foreign import createCanvas       :: forall e. String -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createRenderer     :: forall e. VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createCtx          :: forall e. VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createStave        :: forall e. Int -> Number -> Number -> Eff (vexFlow :: VEXFLOW | e) VexFlow
-- Eff in DOM
foreign import drawStave          :: forall e. VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawKeyStave       :: forall e. VexFlow -> Clef -> VexFlow  -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import createKeySignature :: forall e. KeySignature -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createTimeSignature :: forall e. TimeSignature -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createNotes        :: forall e. VexFlowBar -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addAccidentals     :: forall e. VexFlow -> Array (Array (Array (Tuple String Int))) -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addTies            :: forall e. VexFlow -> Array Int  -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addBeams           :: forall e. VexFlow -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import createNewVoice     :: forall e. Number  -> Number -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import addNotesToVoice    :: forall e. VexFlow -> (Eff (vexFlow :: VEXFLOW) VexFlow) -> Eff (vexFlow :: VEXFLOW | e) VexFlow
foreign import formatter          :: forall e. VexFlow -> Number -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawVoice          :: forall e. VexFlow -> VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawBeams          :: forall e. VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawTies           :: forall e. VexFlow -> VexFlow -> Eff (vexFlow :: VEXFLOW | e) Unit

foreign import logger             :: forall a. a -> forall e. Eff (dom :: DOM | e) Unit
