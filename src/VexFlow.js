
// module VexFlow

module.exports = {

    createCanvas: (function(div) {
    	return function(){
    	    return document.getElementById(div);
    	};
    }),

    createRenderer: (function(canvas) {
    	return function() {;
    	    var renderer = new Vex.Flow.Renderer(canvas, Vex.Flow.Renderer.Backends.CANVAS);
    	    return renderer;
    	};
    }),

    createCtx: (function(renderer) {
    	return function() {
    	    return renderer.getContext();
    	};
    }),

    createStave: (function(x) {
    	return function(y) {
    	    return function(width) {
    		return function() {
    		    var stave = new Vex.Flow.Stave(x, y, width);
    		    return stave;
    		};
    	    };
	
    	};
    }),

    drawKeyStave: function(stave) {
    	return function(clef) {
    	    return function(ctx) {
    		return function() {
    		    stave.addClef(clef).setContext(ctx).draw();
    		};
    	    };
    	};
    },

    drawStave: function(stave) {
    	    return function(ctx) {
    		return function() {
		    stave.setContext(ctx).draw();
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

    createTimeSignature: function(meter) {
	return function (stave) {
	    console.log(stave);
	    return function() {
		return stave.addTimeSignature(meter);
	    };
	};
    },

    logger: function(item) {
	return function() {
	    console.log("Logger :" + (item));
	};
    },
    
    createNotes: function(voices) {
    	    return function() {
    		return voices.map(function(voice){
    		    return voice.map(function(note){
    			return (new Vex.Flow.StaveNote({ keys: note.pitch, duration: note.duration}));
			
    		    });
    		});
    	    };
    },

    addAccidentals: function(voices) {
	console.log(voices);
    	return function(indexList) {
    	    return function() {
		 return indexList.map(function(e, i) {
		    return indexList[i].map(function(f, j) {;
    			function addAccidental (prev, curr){
    			    return prev.addAccidental(curr.value0, new Vex.Flow.Accidental(curr.value1));
			};
			return indexList[i][j].reduce(addAccidental, voices[i][j]);
		    });
		 });
    	    };	
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

    addBeams: function(voices){
	console.log("Add beams");
	console.log(voices);
	return function(){
	    return voices.map(function(voice){
		return new Vex.Flow.Beam(voice);
	    	});
	};
    },

    addTies: function(voices){
	console.log("Add ties");
	console.log(voices);
	return function(){
	    return voices.map(function(voice, i){
		return new Vex.Flow.StaveTie({
		    first_note: voice[i],
		    last_note: voice[i+1],
		    first_indices: [0],
		    last_indices: [0]
		});
		
	    });
	};
    },    
    
    addNotesToVoice: function(notes) {
    	return function(voice) {
    		return function() {
    		    console.log(("Voice : " + voice));
    		    console.log((notes));
    		    return notes.map(function(note) {
    			return voice().addTickables(note);
    		    });
    		};
    	};
    },

    formatter: function(voices) {
    	return function(pxRes) {
    	    return function() {
		console.log("Formatter: ");
		console.log(voices);
    		return new Vex.Flow.Formatter().joinVoices(voices).format(voices, pxRes);
    	    };
    	};
    },
	
    drawVoice: function(ctx) {
	console.log("Drawing voice.");
    	return function(stave) {
    	    return function(voices) {
		console.log(voices);
    		return function() {
    		    voices.map(function(voice) {
    			console.log(voice);
    			return voice.draw(ctx,stave);
    			});
    		};
    	    };
    	};
    },

    drawBeams: function(beams) {
	console.log(beams);
	return function(ctx){
	    return function(){
		beams.map(function(beam){
		    beam.setContext(ctx).draw();
		});
	    };
	};
    }
};


