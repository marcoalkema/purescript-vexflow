
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
	    return function() {
		return stave.addTimeSignature(meter);
	    };
	};
    },

    logger: function(item) {
	return function() {
	    console.log(item);
	};
    },
    
    createNotes: function(voices) {
    	    return function() {
    		return voices.map(function(voice){
    		    return voice.map(function(note){
			if (note.duration.indexOf("d") > 0) {
			    return (new Vex.Flow.StaveNote({ keys: note.pitch, duration: note.duration})).addDotToAll();
			}
			else {
    			    return (new Vex.Flow.StaveNote({ keys: note.pitch, duration: note.duration}));
			}
			
    		    });
    		});
    	    };
    },

    addAccidentals: function(voices) {
    	return function(indexList) {
    	    return function() {
		 return indexList.map(function(e, i) {
		    return indexList[i].map(function(f, j) {;
    			function addAccidental (prev, curr){
    			    return prev.addAccidental(curr.value1, new Vex.Flow.Accidental(curr.value0));
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
    		return (new Vex.Flow.Voice({
    		    num_beats: numBeats,
    		    beat_value: beatValue,
    		    resolution: Vex.Flow.RESOLUTION
    		}));
    	    };
    	};
    },

    addBeams: function(voices){
	return function(indices) {
	    return function() {
		return voices.map(function(voice){
		    return indices.map(function(index){
			if (index.length > 1) {
				console.log(voice);
				console.log(index);
				var start = index[0];
				var end = index[index.length - 1] + 1;
				console.log(start);
				console.log(end);
				var group = voice.slice(start, end);
				console.log(group);
				var kip = new Vex.Flow.Beam(group);
				console.log(kip);
				console.log("mauw");
				return kip;
			    };
		    });
		});
	    };
	};
    },

    addTies: function(voice){
	return function(indices){
	    return function() {
		return indices.map(function(index){
		    return new Vex.Flow.StaveTie({
			first_note: voice[0][index],
			last_note: voice[0][index+1],
			first_indices: [0],
			last_indices: [0]
		    });
		});
	    };
	};
    },    
    
    addNotesToVoice: function(notes) {
    	return function(voice) {
    		return function() {
    		    return notes.map(function(note) {
    			return voice().addTickables(note);
    		    });
    		};
    	};
    },

    formatter: function(voices) {
    	return function(pxRes) {
    	    return function() {
    		return new Vex.Flow.Formatter().joinVoices(voices).format(voices, pxRes);
    	    };
    	};
    },
	
    drawVoice: function(ctx) {
    	return function(stave) {
    	    return function(voices) {
    		return function() {
    		    voices.map(function(voice) {
    			return voice.draw(ctx,stave);
    			});
    		};
    	    };
    	};
    },

    drawTies: function(tiedVoice) {
	return function(ctx){
	    return function(){
		tiedVoice.map(function(voice){
		    return voice.setContext(ctx).draw();
		});
	    };
	};
    },

    drawBeams: function(voices) {
	return function(ctx){
	    return function(){
		console.log(voices);
		voices.map(function(voice){
		    voice.map(function(v) {
			if (v != undefined) {
			    v.setContext(ctx).draw();
			}
		    });
		});
	    };
	};
    }
};


