module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import VexFlow as Vx

main :: forall e. Eff (vexFlow :: Vx.VEXFLOW | e) Unit
main = do
  -- log "hoi"
  canvas <- Vx.createCanvas "#one canvas"
  Vx.createRenderer canvas
  -- ctx <- VX.createCtx renderer
  -- stave <- VX.createStave 1.0 1.0 500.0
  -- VX.drawStave stave "Treble" ctx
  
