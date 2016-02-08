module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import VexFlow as VX

foreign import data VEX :: !

main :: forall e. Eff (vexFlow :: VX.VEXFLOW | e) Unit
main = do
  -- log "hoi"
  VX.createCanvas "#one canvas"
  -- renderer <- VX.createRenderer canvas
  -- ctx <- VX.createCtx renderer
  -- stave <- VX.createStave 1.0 1.0 500.0
  -- VX.drawStave stave "Treble" ctx
  
