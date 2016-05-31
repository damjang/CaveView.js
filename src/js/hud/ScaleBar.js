"use strict";

var CV = CV || {};

CV.ScaleBar = function ( container, hScale, rightMargin ) {

	var leftMargin = 10;

	THREE.Group.call( this );

	this.name = "CV.ScaleBar";
	this.domObjects = [];

	this.hScale        = hScale;
	this.scaleBars     = [];
	this.currentLength = 0;

	this.position.set( -container.clientWidth / 2 +  5,  -container.clientHeight / 2 + leftMargin, 0 );
	this.scaleMax = container.clientWidth - ( leftMargin + rightMargin );

	var legend = document.createElement( "div" );

	legend.classList.add( "scale-legend" );
	legend.textContent = "";

	container.appendChild( legend );

	this.legend = legend;
	this.domObjects.push( legend );

	this.addEventListener( "removed", this.removeDomObjects );

	return this;

}

CV.ScaleBar.prototype = Object.create( THREE.Group.prototype );

Object.assign( CV.ScaleBar.prototype, CV.HudObject.prototype );

CV.ScaleBar.prototype.constructor = CV.ScaleBar;

CV.ScaleBar.prototype.setScale = function ( scale ) {

	var scaleBars = this.scaleBars;
	var length = 0;
	var self   = this;

	var maxVisible = this.scaleMax / ( scale * this.hScale );
	var exponent = Math.ceil( Math.log( maxVisible ) / Math.LN10 ) - 1;
	var rMax     = Math.pow( 10, exponent );
	var maxInc   = maxVisible / rMax;
	var legendText;

	if ( maxInc < 2 ) {

		length = 10;
		exponent = exponent - 1;

	} else if ( maxInc < 5 ) {

		length = 2;

	} else {

		length = 5;

	}

	if ( exponent >= 3 ) {

		legendText = length * Math.pow( 10, exponent - 3) + 'km';

	} else {

		legendText = length * Math.pow( 10, exponent ) + 'm';

	}

	scale = scale * Math.pow( 10, exponent );	

	if ( this.currentLength !== length ) {

		if ( !scaleBars[ length ] ) {

			var bar = _makeScaleBar( length );

			scaleBars[ length ] = bar;
			this.add( bar.mesh );

		}

		if ( this.currentLength > 0 ) {

			scaleBars[ this.currentLength ].mesh.visible = false;

		}

		scaleBars[ length ].mesh.visible = true;
		this.currentLength = length;

	}

	scaleBars[ length ].mesh.scale.x = scale;

	var legend = this.legend;

	legend.style.display = "block";
	legend.style.left = ( scale * scaleBars[ length ].topRight - legend.clientWidth ) + "px";

	legend.textContent = legendText;

	return this;

	function _makeScaleBar ( length ) {

		var height = 4;
		var rLength = length * self.hScale;
		var i, l;

		var bar  = new THREE.PlaneGeometry( rLength, height, length );
		var bar2 = new THREE.PlaneGeometry( rLength, height, length * 10 );
		var line = new THREE.Geometry();

		line.vertices.push( new THREE.Vector3( -rLength / 2, 0, 1 ) );
		line.vertices.push( new THREE.Vector3(  rLength / 2, 0, 1 ) );

		var mBar  = new THREE.Mesh( bar,  new THREE.MeshBasicMaterial( { color: 0xffffff, vertexColors: THREE.FaceColors, side: THREE.FrontSide } ) );
		var mBar2 = new THREE.Mesh( bar2, new THREE.MeshBasicMaterial( { color: 0xffffff, vertexColors: THREE.FaceColors, side: THREE.FrontSide } ) );
		var mLine = new THREE.LineSegments( line, new THREE.LineBasicMaterial( { color: 0xff0000 } ) );

		var cRed = new THREE.Color( 0xff0000 );

		for ( i = 0, l = bar.faces.length; i < l; i = i + 4 ) {

			bar.faces[ i ].color   = cRed;
			bar.faces[ i+1 ].color = cRed;

		}

		for ( i = 0, l = bar2.faces.length; i < l; i = i + 4 ) {

			bar2.faces[ i ].color   = cRed;
			bar2.faces[ i+1 ].color = cRed;

		}

		bar.translate( rLength / 2, height + height / 2 + 1, 0 );
		bar2.translate( rLength / 2, height / 2, 0 );
		line.translate( rLength / 2, height, 0 );

		bar.computeBoundingBox();

		var group = new THREE.Group();

		group.add( mBar );
		group.add( mBar2 );
		group.add( mLine );

		return { mesh: group, topRight: bar.boundingBox.max.x };

	}

}

// EOF