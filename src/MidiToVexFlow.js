// module MidiToVexFlow
var mjt = require('MidiJsTypes');

// MidiEventRec -> MidiEventFoo
module.exports = {

    unsafeF1: function(midiEventRec) {
	var res = null;
	// console.log('unsafeF1: midiEventRec = ', midiEventRec);
	
	    if      (midiEventRec.subtype == 'noteOn')      {
		res = new mjt.NoteOn(midiEventRec);
	}
	    else if (midiEventRec.subtype == 'noteOff')      {
		res = new mjt.NoteOff(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'timeSignature')      {
		res = new mjt.TimeSignature(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'keySignature')      {
		res = new mjt.KeySignature(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'smpteOffset')      {
		res = new mjt.SmpteOffset(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'setTempo')      {
		res = new mjt.SetTempo(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'endOfTrack')      {
		res = new mjt.EndOfTrack(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'programChange')      {
		res = new mjt.ProgramChange(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'controller')      {
		res = new mjt.Controller(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'trackName')      {
		res = new mjt.TrackName(midiEventRec);
	    }
	    else if (midiEventRec.subtype == 'instrumentName') {
		res = new mjt.InstrumentName(midiEventRec);
	    }
	else if (midiEventRec.subtype == 'midiChannelPrefix') {
		res = new mjt.MidiChannelPrefix(midiEventRec);
	    }
	else if (midiEventRec.subtype == 'marker') {
	    res = new mjt.Marker(midiEventRec);
	}
	
	console.log('res =', res);
	if (!res) throw new Error('Pattern match on subtype field failed.');
	    return res;
    }
};
