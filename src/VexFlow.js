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
	return function() {
	    console.log ("Creating context for: " + renderer);
	    return renderer.getContext();
	};
    }),

    createStave: (function(x) {
	return function(y) {
	    return function(width) {
		return function() {
		    console.log ("createStave");
		    var stave = new Vex.Flow.Stave(x, y, width);
		    return stave;
		};
	    };
	
	};
    }),

    drawStave: function(stave) {
	return function(clef) {
	    return function(ctx) {
		return function() {
		    console.log(stave);
		    console.log(clef);
		    console.log(ctx);
		    stave.addClef(clef).setContext(ctx).draw();
		};
	    };
	};
    },

        createNotes: function(notes) {
	    console.log (notes);
	    return function() {
		return notes.map(function(note){
		    return (new Vex.Flow.StaveNote({ keys: note.pitch, duration: note.duration}));
		});
	    };
	},
			 
			 
			  
			   

    createNewVoice: function(numBeats) {
	return function(beatValue) {
	    return function() {
		console.log("CreateNewVoice");
		var voice = new Vex.Flow.Voice({
		    num_beats: numBeats,
		    beat_value: beatValue,
		    resolution: Vex.Flow.RESOLUTION
		});
		return voice;
	    };
	};
    },

    addNotesToVoice: function(notes) {
	return function(voice) {
	    return function() {
		console.log (voice + " " + notes);
		voice.addTickables(notes);
	    };
	};
    },

    // Format and justify the notes to 500 pixels
    formatter: function(voice) {
	return function(pxRes) {
	    return function() {
		console.log ("Formatter: " + voice + " " + pxRes);
		var formatter = new Vex.Flow.Formatter().joinVoices([voice]).format([voice], pxRes);
		return formatter;
	    };
	};
    },
	
    drawVoice: function(ctx) {
	return function(stave) {
	    return function(voice) {
		return function() {
		    console.log(ctx + stave + voice);
		    voice.draw(ctx, stave);
		};
	    };
	};
    }
};
