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

    createKeySignature: function(key) {
	return function (stave) {
	    return function() {
		return ((new Vex.Flow.KeySignature(key)).addToStave(stave));
	    };
	};
    },

    logger: function(item) {
	return function() {
	    console.log("Logger :" + JSON.stringify(item));
	};
    },
    
    createNotes: function(voices) {
    	    return function() {
    		return voices.map(function(voice){
    		    console.log("createNotes " + (JSON.stringify(voice)));
    		    return voice.map(function(note){
    			return (new Vex.Flow.StaveNote({ keys: note.pitch, duration: note.duration}));	
    		    });
    		});
    	    };
    },
			 
    createNewVoice: function(numBeats) {
    	return function(beatValue) {
    	    return function() {
    		console.log("CreateNewVoice in: " + numBeats + " : " + beatValue);
    		return (new Vex.Flow.Voice({
    		    num_beats: numBeats,
    		    beat_value: beatValue,
    		    resolution: Vex.Flow.RESOLUTION
    		}));
    	    };
    	};
    },

    addNotesToVoice: function(notes) {
    	return function(voice) {
    		return function() {
    		    console.log(voice);
    		    console.log(notes);
    		    return notes.map(function(note) {
    			// console.log (JSON.stringify(note, null, 4));
    			return voice().addTickables(note);
    		    });
    		};
    	};
    },

    // Format and justify the notes to 500 pixels
    formatter: function(voices) {
    	return function(pxRes) {
    	    return function() {
    		console.log("Formatter :" + voices + " - " + "Pixel resolution : " + pxRes);
    		// console.log(JSON.stringify(voices));
    		var formatter = new Vex.Flow.Formatter().joinVoices(voices).format(voices, pxRes);
    		return formatter;
    	    };
    	};
    },
	
    drawVoice: function(ctx) {
    	return function(stave) {
    	    return function(voices) {
    		return function() {
    		    voices.map(function(voice) {
    			console.log(voice);
    			return voice.draw(ctx,stave);
    			});
    		};
    	    };
    	};
    }
};
