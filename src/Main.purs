module Main where

import Prelude
import Data.List.Lazy
import Data.Tuple
import Music as M
-- import Control.Monad.Eff
-- import Control.Monad.Eff.Console
import VexFlow as Vx

type Voice2 = List Vx.VexNote

main :: Vx.VexFlowEff
main = do
  canvas <- Vx.createCanvas "notationCanvas"
  renderer <- Vx.createRenderer canvas
  ctx <- Vx.createCtx renderer
  stave <- Vx.createStave 1.0 1.0 500.0
  Vx.createKeySignature "D" stave
  Vx.drawStave stave "treble" ctx
  notes <- Vx.createNotes voicesCis
  voicing <- Vx.addNotesToVoice notes (Vx.createNewVoice 4.0 4.0)  
  Vx.formatter voicing 500.0
  Vx.drawVoice ctx stave voicing
  Vx.logger Music.indexedCis
  
voices :: Vx.Bar
voices = [[{pitch: ["c/4", "f/4", "g/4"], duration: "h"}
          ,{pitch: ["b/5"], duration: "h"}
           ]
         , [{pitch: ["ab/5"], duration: "1"}]
         , [{pitch: ["c#/5"], duration: "2"}
           ,{pitch: ["d##/5"], duration: "8"}
           ,{pitch: ["gbb/5"], duration: "8"}
           ,{pitch: ["d##/5"], duration: "8"}
           ,{pitch: ["gbb/5"], duration: "8"}
           ]
          ]

voicesCis :: Vx.Bar
voicesCis = [[M.vexCis]]

listVoice :: List Vx.Bar
listVoice = Data.List.Lazy.toList [voices]

barIndex :: List Vx.Bar -> List (Tuple Int Vx.Bar)
barIndex voices' = Data.List.Lazy.zip (Data.List.Lazy.iterate (+1) 0) voices'
-- Zip lazy list with individual notes, not bars, maybe voices

-- listVoice :: List VexNote
-- listVoice voice = Data.List.Lazy.toList voice

-- voiceIndex :: Vx.Note -> List (Tuple Int Vx.Voice)
-- voiceIndex voice = Data.List.Lazy.zip (Data.List.Lazy.iterate (+1) 0) voice

-- accidentals :: forall a b. Array (Tuple a b) -> List (Array Int)
-- accidentals voice = map (\Tuple a b -> if a.accidental /= M.Natural then [b] else []) voice

-- hasAccidental :: Voice -> Array Int
-- hasAccidental voice = if voice.accidental != natural then
--                         true
--                       else
--                         false

-- accidentalIndex voices = foldl (\voice accumulator -> if voice.accidental != Natural then index

-- infiniteList :: Int -> Array Int
-- infiniteList init = init Data.Array.cons map (+1) infiniteList
