module Piano where

import Prelude
import Prelude (map)
import Math
import Math (max)
import Data.Int (round, toNumber)
import Data.Array
import Data.Array
import Pux.Html as Pux
import Pux.Html.Events
import Pux.Html.Elements
import Pux.Html.Elements (span)
import Pux.Html.Attributes
import Pux.Html.Attributes (style)

data Action = Increment | Decrement

data Note = NoteC | NoteCis | NoteD | NoteDis | NoteE | NoteF | NoteFis | NoteG | NoteGis | NoteA | NoteAis | NoteB

type State = { a :: Number
             , b :: Octave }

type Octave = Int

init :: State
init = { a : 0.0
       , b : 0 }

update :: Action -> State -> State
update Increment state = state { a = 5.0 }
update Decrement state = state { a = 3.0 }

view :: State -> Html Action
view state =
  Pux.div
    []
    [ button [ onClick (const Increment) ] [ text "Increment" ]
    , span [] [ text (show state.a) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    , kip
    ]
    
kip :: Html Action
kip = Pux.div [] drawOctaves

octaveNumber :: Int
octaveNumber = 5

drawOctaves :: Array (Html Action)
drawOctaves = map drawOctave $ octaves 5

octaves :: Octave -> Array Octave
octaves n = range 1 (round (max (toNumber 1) (abs $ toNumber n)))

drawOctave :: Octave  -> (Html Action)
drawOctave oct = Pux.div [] (drawKeys oct)

drawKeys :: Octave -> Array (Html Action)
drawKeys octave = map (\x -> drawKey x octave) positions

-- drawKey :: PosRec -> Octave -> Html                          
-- drawKey posRec octave  = (Pux.div [onClick (const Increment), styles posRec.isBlack posRec.position octave] [])

drawKey :: PosRec -> Octave -> Html Action
drawKey posRec octave  = (Pux.div [ onClick (const Increment) ] [])


-- styles :: Boolean -> Number -> Octave -> Attribute
-- styles isBlack = if isBlack then styleBlack else styleWhite

-- styles :: Boolean -> Number -> Octave -> Attribute
-- styles isBlack n oct = if isBlack then styles else styles

-- data Color = Black | White

-- styles :: Boolean -> Number -> Octave -> Color
-- styles isBlack = if isBlack then Black else White

type PosRec = { position :: Number
              , isBlack :: Boolean
              , note :: Note }

pos0 ::  PosRec
pos0 = { position   : 0.0 * whiteKeyWidth
       , isBlack    : false
       , note       : NoteC }
pos1 ::  PosRec
pos1 =  { position  : blackKeyOffset + (0.0 * whiteKeyWidth)
        , isBlack   : true
        , note      : NoteCis }
pos2 ::  PosRec
pos2 =  { position  : 1.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteD }
pos3 ::  PosRec
pos3 =  { position  : blackKeyOffset + (1.0 * whiteKeyWidth)
        , isBlack   : true
        , note      :  NoteDis }
pos4 ::  PosRec
pos4 =  { position  : 2.0 * whiteKeyWidth
        , isBlack      : false
        , note      : NoteE }
pos5 ::  PosRec
pos5 =  { position  : 3.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteF }
pos6 ::  PosRec
pos6 =  { position  : blackKeyOffset + (3.0 * whiteKeyWidth)
        , isBlack   :  true
        , note      :  NoteFis }
pos7 ::  PosRec
pos7 =  { position  : 4.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteG }
pos8 ::  PosRec
pos8 =  { position  : blackKeyOffset + (4.0 * whiteKeyWidth)
        , isBlack   : true
        , note      : NoteGis }
pos9 ::  PosRec
pos9 =  { position  : 5.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteA }
pos10 ::  PosRec
pos10 =  { position : blackKeyOffset + (5.0 * whiteKeyWidth)
         , isBlack  : true
         , note     : NoteAis }
pos11 ::  PosRec
pos11 = { position  : 6.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteB }

positions :: Array PosRec
positions = [pos0, pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8, pos9, pos10, pos11]

whiteKeyWidth :: Number
whiteKeyWidth = 100.0 / 7.0
blackKeyOffset :: Number
blackKeyOffset = (whiteKeyWidth * 0.7)

-- containerStyle :: Attribute Action
containerStyle =
  style { height : "100%", width : "100%" }
    -- [ ("height", "100%")
    -- , ("width", "100%") 
    -- ]

-- whiteKeyHeight :: Number
-- whiteKeyHeight = (6.7342 * (whiteKeyWidth / abs octaveNumber))

-- styleWhite :: Number -> Int -> Attribute
-- styleWhite pos octave =
--   style
--     [ ("background-color", "white")
--     , ("border", "2px solid #aaa")
--     , ("height", toString whiteKeyHeight ++ "%")
--     , ("width", toString (whiteKeyWidth / abs octaveNumber) ++ "%")
--     , ("left", toString (((100.0 / abs octaveNumber) * (toNumber octave - 1.0)) + (pos / abs octaveNumber)) ++ "%")
--     , ("position", "absolute")
--     ]

-- styleBlack :: Number -> Int -> Attribute
-- styleBlack pos octave =
--   style
--     [ ("background-color", "black")
--     , ("color", "red")
--     , ("height", (toString (0.7 * whiteKeyHeight)) ++ "%")
--     , ("width", toString ((whiteKeyWidth * 0.6) / abs octaveNumber) ++ "%")
--     , ("z-index", "1")
--     , ("position" , "absolute")
--     , ("left" , toString (((100.0 / abs octaveNumber) * (toNumber octave - 1.0)) +  (pos / abs octaveNumber)) ++ "%")
--     ]

-- notationStyle :: Attribute
-- notationStyle =
--   style
--     [ ("height", "100px")
--     , ("width", "74.8%")
--     , ("background-color", "#ddd")
--     , ("border", "2px solid #aaa")
--     , ("top", "200px")
--     , ("position", "relative")
--     , ("font-size", "72px")
--     , ("float", "left")
--     , ("overflow", "scroll")  
--     ]

-- buttonStyle :: Attribute
-- buttonStyle =
--   style
--     [ ("height", "104px")
--     , ("width", "60px")
--     , ("background-color", "#ddd")
--     , ("border", "2px solid #aaa")
--     , ("top", "200px")
--     , ("position", "relative")
--     , ("font-size", "72px")
--     , ("float", "left")
--     , ("left", "10px")
--     , ("z-index", "4")  
--     ]

-- notationStyle' :: Attribute
-- notationStyle' =
--   style
--     [ ("height", "360px")
--     , ("width", "98%")
--     , ("border", "2px solid #aaa")
--     , ("top", "210px")
--     , ("position", "relative")
--     , ("font-size", "72px")
--     , ("float", "left")
--     , ("left", "10px")
--     , ("z-index", "4")  
--     ]                  
