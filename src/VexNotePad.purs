module NotePad where
  
    
-- vexBar :: Vm.VexFlowBar
-- vexBar = [[{pitch: ["c/4", "f/4", "g/4"], duration: "h"}, {pitch: ["b/5"], duration: "h"}]           
--          ,[{pitch: ["ab/5"], duration: "1"}]          
--          ,[{pitch: ["c/5"], duration: "2"}
--            ,{pitch: ["d/5"], duration: "8"}
--            ,{pitch: ["g/5"], duration: "8"}
--            ,{pitch: ["d/5"], duration: "8"}
--            ,{pitch: ["g/5"], duration: "8"}
--            ]
--           ]

-- vexBar2 :: Vm.VexFlowBar
-- vexBar2 =  [[{pitch: ["d/4"], duration: "8"}
--             ,{pitch: ["g/4"], duration: "8"}
--             ,{pitch: ["d/4"], duration: "8"}
--             ,{pitch: ["g/4"], duration: "8"}
--             ,{pitch: ["g/4"], duration: "8"}
--             ,{pitch: ["d/4"], duration: "8"}
--             ,{pitch: ["g/4"], duration: "8"}
--             ,{pitch: ["g/4"], duration: "8"}]]

-- vexAccidentalBar2 :: Array (Array (Array (Tuple String Int)))
-- vexAccidentalBar2 = [[[],[],[],[],[],[],[],[]]]

-- vexAccidentalBar :: Array (Array (Array (Tuple Int String)))
-- vexAccidentalBar = [ [[(Tuple 0 "#"), (Tuple 1 "b")], [(Tuple 0 "#")]]
--                    , [[Tuple 0 "b"]]
--                    , [[(Tuple 0 "#")], [], [], [(Tuple 0 "b")], [(Tuple 0 "b")]]
--                    ]
                      
-- testVoice :: Vm.VexFlowVoice
-- testVoice = [{pitch: ["c#/5"], duration: "2"}
--             ,{pitch: ["d##/5"], duration: "8"}
--             ,{pitch: ["gbb/5"], duration: "8"}
--             ,{pitch: ["d##/5"], duration: "8"}
--             ,{pitch: ["gbb/5"], duration: "8"}
--             ]

-- voicesCis :: Vm.VexFlowBar
-- voicesCis = [[]]


-- drawTrebleStaveLine :: Number -> Vm.VexFlowVoice -> Vx.VexFlow -> Vx.VexFlowEff    
-- drawTrebleStaveLine y notes renderer = do
--   drawTrebleStave y renderer "G"
--   let trebleStave1 = drawStave renderer 80.0 y 280.0
--   trebleStave1 drawVoice 
--   let trebleStave2 = drawStave renderer 360.0 y 280.0
--   trebleStave2 drawVoice
--   let trebleStave3 = drawStave renderer 640.0 y 280.0
--   trebleStave3 drawVoice
--   let trebleStave4 = drawStave renderer 920.0 y 280.0
--   trebleStave4 drawVoice

-- drawBassStaveLine :: Number -> Vm.VexFlowVoice -> Vx.VexFlow -> Vx.VexFlowEff    
-- drawBassStaveLine y notes renderer = do
--   drawBassStave y renderer "G"
--   let bassStave1 = drawStave renderer 80.0 y 280.0
--   bassStave1 drawVoice
--   let bassStave2 = drawStave renderer 360.0 y 280.0
--   bassStave2 drawVoice
--   let bassStave3 = drawStave renderer 640.0 y 280.0
--   bassStave3 drawVoice
--   let bassStave4 = drawStave renderer 920.0 y 280.0
--   bassStave4 drawVoice

-- drawKeyedStave :: Vx.VexFlow -> Number -> Number -> Number -> KeySignature -> (Vx.VexFlow -> Vx.VexFlow -> Vx.VexFlowEff) -> Vx.VexFlowEff
-- drawKeyedStave renderer x y w key voice = do
--   ctx <- Vx.createCtx renderer
--   stave <- Vx.createStave x y w
--   Vx.createKeySignature key stave
--   Vx.drawStave stave ctx
--   voice ctx stave
