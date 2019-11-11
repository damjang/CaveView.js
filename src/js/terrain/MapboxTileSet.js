
const halfMapExtent = 6378137 * Math.PI; // from EPSG:3875 definition

var tileSets;

function MapboxTileSet( tileSetReady ) {

	tileSets = [ MapboxTileSet.defaultTileSet ];

	setTimeout( tileSetReady );

}

MapboxTileSet.defaultTileSet = {
	isFlat: false,
	title: 'Mapbox',
	overlayMaxZoom: 15,
	maxZoom: 15,
	minZoom: 15,
	divisions: 255,
	subdirectory: null,
	dtmScale: 1,
	minX: 0,
	maxX: 32767,
	minY: 0,
	maxY: 32767,
	attributions: [],
	log: true
};

MapboxTileSet.prototype.workerScript = 'mapboxTileWorker.js';

MapboxTileSet.prototype.getTileSets = function () {

	return tileSets;

};

MapboxTileSet.prototype.getScreenAttribution = function () {

	return null;

};

MapboxTileSet.prototype.getCoverage = function ( limits, zoom ) {

	const coverage = { zoom: zoom };

	const N =  halfMapExtent;
	const W = -halfMapExtent;

	const tileCount = Math.pow( 2, zoom - 1 ) / halfMapExtent; // tile count per metre

	coverage.min_x = Math.floor( ( limits.min.x - W ) * tileCount );
	coverage.max_x = Math.floor( ( limits.max.x - W ) * tileCount );

	coverage.max_y = Math.floor( ( N - limits.min.y ) * tileCount );
	coverage.min_y = Math.floor( ( N - limits.max.y ) * tileCount );

	coverage.count = ( coverage.max_x - coverage.min_x + 1 ) * ( coverage.max_y - coverage.min_y + 1 );

	return coverage;

};

MapboxTileSet.prototype.getTileSpec = function ( x, y, z, limits ) {

	const tileSet = this.tileSet;
	const scale = ( z > tileSet.maxZoom ) ? Math.pow( 2, tileSet.maxZoom - z ) : 1;

	// don't zoom in with no overlay - no improvement of terrain rendering in this case

	if ( scale !== 1 && this.activeOverlay === null ) return null;

	if ( this.log ) console.log( 'load: [ ', z +'/' + x + '/' + y, ']' );

	const tileWidth = halfMapExtent / Math.pow( 2, z - 1 );

	const clip = { top: 0, bottom: 0, left: 0, right: 0 };

	const tileMinX = tileWidth * x - halfMapExtent;
	const tileMaxX = tileMinX + tileWidth;

	const tileMaxY = halfMapExtent - tileWidth * y;
	const tileMinY = tileMaxY - tileWidth;

	const divisions = ( tileSet.divisions ) * scale ;
	const resolution = tileWidth / divisions;

	// trim excess off sides of tile where overlapping with region

	if ( tileMaxY > limits.max.y ) clip.top = Math.floor( ( tileMaxY - limits.max.y ) / resolution );

	if ( tileMinY < limits.min.y ) clip.bottom = Math.floor( ( limits.min.y - tileMinY ) / resolution );

	if ( tileMinX < limits.min.x ) clip.left = Math.floor( ( limits.min.x - tileMinX ) / resolution );

	if ( tileMaxX > limits.max.x ) clip.right = Math.floor( ( tileMaxX - limits.max.x ) / resolution );

	if ( clip.top >= divisions || clip.bottom >= divisions || clip.left >= divisions || clip.right >= divisions ) return null;

	const clippedFraction = ( divisions - clip.top - clip.bottom ) * (divisions - clip.left - clip.right ) / ( divisions * divisions );

	return {
		tileSet: tileSet,
		divisions: divisions,
		resolution: resolution,
		x: x,
		y: y,
		z: z,
		clip: clip,
		offsets: null,
		flatZ: null,
		clippedFraction: clippedFraction,
		request: 'tile'
	};

};

MapboxTileSet.prototype.findTile = function ( point ) {

	const tileSet = this.tileSet;

	const tileWidth = halfMapExtent / Math.pow( 2, tileSet.maxZoom - 1 );

	const xTc = ( point.x + halfMapExtent ) / tileWidth;
	const yTc = ( halfMapExtent - point.y ) / tileWidth;

	const tileX = Math.floor( xTc );
	const tileY = Math.floor( yTc );
	const tileZ = tileSet.maxZoom;

	const offsetX = xTc - tileX;
	const offsetY = yTc - tileY;

	const samples = tileSet.divisions + 1;
	const dataOffset = Math.floor( samples * offsetX ) + samples * Math.floor( samples * offsetY - 1 );

	// construct a tileSpec for passing to web worker
	return {
		x: tileX,
		y: tileY,
		z: tileZ,
		tileSet: tileSet,
		dataOffsets: [ dataOffset ],
		points: [ point ],
		request: 'height',
		clip: {}
	};

};

export { MapboxTileSet };

// EOF