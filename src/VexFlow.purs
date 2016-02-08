module VexFlow where

import Prelude
import Control.Monad.Eff
import Data.List

foreign import data VEXFLOW :: !

type Div = String
type Clef = String
type Note = String
type Octave = Number
type Duration = Number

foreign import createCanvas :: forall e. Div -> (Eff (vexFlow :: VEXFLOW | e) Unit)
-- Needs to be JS effect
foreign import createRenderer :: forall e. (Div -> (Eff (vexFlow :: VEXFLOW | e) Unit)) -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import createCtx  :: forall e. ((Div -> (Eff (vexFlow :: VEXFLOW | e) Unit)) -> Eff (vexFlow :: VEXFLOW | e) Unit) -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import createStave  :: forall e. Number -> Number -> Number -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawStave  :: forall e. Clef -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import createNote  :: forall e. List Note -> Octave -> Duration -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import createNewVoice  :: forall e. Number -> Number -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import addNotesToVoice  :: forall e. (List Note -> Octave -> Duration -> Eff (vexFlow :: VEXFLOW | e) Unit) -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import formatter  :: forall e. ((List Note -> Octave -> Duration -> Eff (vexFlow :: VEXFLOW | e) Unit) -> Eff (vexFlow :: VEXFLOW | e) Unit) -> Eff (vexFlow :: VEXFLOW | e) Unit
foreign import drawVoice  :: forall e. (Eff (vexFlow :: VEXFLOW | e) Unit) -> (Clef -> Eff (vexFlow :: VEXFLOW | e) Unit) -> Eff (vexFlow :: VEXFLOW | e) Unit
