// module VexFlow

module.exports = {

    createCanvas: (function(div) {
	return function(){
	    return document.getElementById(div);
	};
    }),

    createRenderer: (function(canvas) {
	return function() {
	    console.log(canvas);
	    var renderer = new Vex.Flow.Renderer(canvas, Vex.Flow.Renderer.Backends.CANVAS);
	    return renderer;
	};
    }),

    createCtx: (function(renderer) {
	return renderer.getContext();
    }),

    createStave: (function(x, y, width) {
	var stave = new Vex.Flow.Stave(x, y, width);
	return stave;
    }),

    drawStave: function(stave, clef, ctx) {
	stave.addClef(clef).setContext(ctx).draw();
    },

    createNote: function(notes, octave, duration) {
    	var note = new Vex.Flow.StaveNote({ keys: notes, duration: duration });
	return note;
    },

    createNewVoice: function(numBeats, beatValue) {
	var voice = new Vex.Flow.Voice({
	    num_beats: numBeats,
	    beat_value: beatValue,
	    resolution: Vex.Flow.RESOLUTION
	});
	return voice;
    },

    addNotesToVoice: function(notes) {
	voice.addTickables(notes);
    },

    // Format and justify the notes to 500 pixels
    formatter: function(voices, pxRes) {	
	var formatter = new Vex.Flow.Formatter().joinVoices(voices).format(voices, pxRes);
	return formatter;
	
    },
	
    drawVoice: function(ctx, stave) {
	voice.draw(ctx, stave);
    }
    
};
