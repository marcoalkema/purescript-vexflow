module Quantizer where

import Prelude
import MidiPlayer
import MidiJsTypes

-- TODO: quantize: triplets, sixteenths

quantizeNote :: Number -> Number -> MidiNote -> MidiNote
quantizeNote ticksPerBeat acc event
  | event.deltaTime < (acc * ticksPerBeat / 4.0) = event {deltaTime = acc * (ticksPerBeat / 4.0)}

quantizeNote ticksPerBeat acc event
  | event.deltaTime > (acc * ticksPerBeat / 4.0)
    && event.deltaTime < (acc * (ticksPerBeat / 4.0) + (ticksPerBeat / 8.0)) = event {deltaTime = acc * (ticksPerBeat / 4.0)}

quantizeNote ticksPerBeat acc event
  | event.deltaTime > (acc * ticksPerBeat / 4.0)
    && event.deltaTime > (acc * (ticksPerBeat / 4.0) + (ticksPerBeat / 8.0)) = quantizeNote ticksPerBeat (acc + 1.0) event

quantizeNote ticksPerBeat acc event = event

