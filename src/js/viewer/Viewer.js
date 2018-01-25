
import  {
	VERSION,
	CAMERA_ORTHOGRAPHIC, CAMERA_PERSPECTIVE, CAMERA_OFFSET,
	FACE_WALLS, FACE_SCRAPS, FEATURE_TRACES,
	LEG_CAVE, LEG_SPLAY, LEG_SURFACE, LABEL_STATION,
	SHADING_HEIGHT, SHADING_SINGLE, SHADING_SHADED, SHADING_OVERLAY, SHADING_PATH,
	SHADING_DEPTH, SHADING_DEPTH_CURSOR,
	FEATURE_BOX, FEATURE_ENTRANCES, FEATURE_SELECTED_BOX, FEATURE_TERRAIN, FEATURE_STATIONS,
	VIEW_ELEVATION_N, VIEW_ELEVATION_S, VIEW_ELEVATION_E, VIEW_ELEVATION_W, VIEW_PLAN, VIEW_NONE,
	upAxis,
	MOUSE_MODE_ROUTE_EDIT, MOUSE_MODE_NORMAL
} from '../core/constants';

import { HUD } from '../hud/HUD';
import { Materials } from '../materials/Materials';
import { CameraMove } from './CameraMove';
import { CaveLoader } from '../loaders/CaveLoader';
import { Survey } from './Survey';
import { StationPopup } from './StationPopup';
import { WebTerrain } from '../terrain/WebTerrain';
import { Overlay } from '../terrain/Overlay';
import { Cfg } from '../core/lib';

// analysis tests
//import { DirectionGlobe } from '../analysis/DirectionGlobe';
//import { ClusterLegs } from '../analysis/ClusterLegs';

import { OrbitControls } from '../core/OrbitControls';

import {
	EventDispatcher,
	Vector2, Vector3, Matrix4, Euler,
	Scene, Raycaster,
	DirectionalLight, HemisphereLight,
	LinearFilter, NearestFilter, RGBAFormat,
	OrthographicCamera, PerspectiveCamera,
	WebGLRenderer, WebGLRenderTarget,
	MOUSE
} from '../../../../three.js/src/Three';

const defaultView = {
	autoRotate: false,
	autoRotateSpeed: 0.5,
	box: true,
	view: VIEW_PLAN,
	cameraType: CAMERA_PERSPECTIVE,
	shadingMode: SHADING_HEIGHT,
	terrainShadingMode: SHADING_SHADED,
	surfaceLegs: false,
	walls: false,
	scraps: false,
	splays: false,
	stations: false,
	stationLabels: false,
	entrances: true,
	terrain: false,
	traces: false,
	HUD: true
};

const renderer = new WebGLRenderer( { antialias: true } ) ;

const defaultTarget = new Vector3();
const lightPosition = new Vector3( -1, -1, 0.5 );
const directionalLight = new DirectionalLight( 0xffffff );
const scene = new Scene();
const mouse = new Vector2();
const raycaster = new Raycaster();

const formatters = {};

const RETILE_TIMEOUT = 150; // ms pause after last movement before attempting retiling

var caveIsLoaded = false;

var container;

// THREE.js objects

var oCamera;
var pCamera;

var camera;

var mouseMode = MOUSE_MODE_NORMAL;
var mouseTargets = [];

var terrain = null;
var survey;
var limits = null;
var stats = {};
var zScale;
var caveLoader;

var cursorHeight;

var shadingMode = SHADING_SINGLE;
var surfaceShadingMode = SHADING_SINGLE;
var terrainShadingMode = SHADING_SHADED;

var overlays = {};
var activeOverlay = null;

var cameraMode;
var selectedSection = 0;

var controls;
var renderRequired = false;

var cameraMove;

var lastActivityTime = 0;

var popup = null;

//var leakWatcher;

const Viewer = Object.create( EventDispatcher.prototype );

function init ( domID, configuration ) { // public method
/*
	WeakMap.prototype.__get = WeakMap.prototype.get;

	WeakMap.prototype.get = function ( obj ) {

		console.log( Object.getPrototypeOf( obj ).constructor.name );
		return this.__get( obj );

	};
*/
	console.log( 'CaveView v' + VERSION );

	container = document.getElementById( domID );

	if ( ! container ) alert( 'No container DOM object [' + domID + '] available' );

	Cfg.set( configuration );

	const width  = container.clientWidth;
	const height = container.clientHeight;

	renderer.setSize( width, height );
	renderer.setPixelRatio( window.devicePixelRatio );
	renderer.setClearColor( Cfg.themeValue( 'background' ) );
	renderer.autoClear = false;

	oCamera = new OrthographicCamera( -width / 2, width / 2, height / 2, -height / 2, 1, 4000 );

	oCamera.rotateOnAxis( upAxis, Math.PI / 2 );

	initCamera( oCamera );

	pCamera = new PerspectiveCamera( 75, width / height, 1, 16000 );

	initCamera( pCamera );

	camera = pCamera;

	scene.add( pCamera );
	scene.add( oCamera );

	directionalLight.position.copy( lightPosition );

	scene.add( directionalLight );

	scene.add( new HemisphereLight( 0xffffff, 0xffffff, 0.3 ) );
	//	scene.autoUpdate = false; // FIXME - update entrance labels/clusters manually

	raycaster.params.Points.threshold = 3;

	renderer.clear();

	container.appendChild( renderer.domElement );

	controls = new OrbitControls( camera, renderer.domElement );

	cameraMove = new CameraMove( controls, renderView, onCameraMoveEnd );

	controls.addEventListener( 'change', function () { cameraMove.prepare( null, null ); cameraMove.start( 80 ); } );

	controls.enableDamping = true;

	// event handler
	window.addEventListener( 'resize', resize );

	Object.defineProperties( Viewer, {

		'container': {
			value: container
		},

		'terrain': {
			writeable: true,
			get: function () { return testCameraLayer( FEATURE_TERRAIN ); },
			set: loadTerrain
		},

		'terrainShading': {
			writeable: true,
			get: function () { return terrainShadingMode; },
			set: function ( x ) { _stateSetter( setTerrainShadingMode, 'terrainShading', x ); }
		},

		'hasTerrain': {
			get: function () { return !! terrain; }
		},

		'terrainAttributions': {
			get: function () { return terrain.attributions; }
		},

		'terrainDatumShift': {
			writeable: true,
			get: function () { return !! terrain.activeDatumShift; },
			set: applyTerrainDatumShift
		},

		'terrainOverlays': {
			get: function () { if ( terrain.isTiled ) return Object.keys( overlays ); else return terrain.hasOverlay ? [ true ] : []; }
		},

		'terrainOverlay': {
			writeable: true,
			get: function () { return activeOverlay; },
			set: function ( x ) { _stateSetter( setTerrainOverlay, 'terrainOverlay', x ); }
		},

		'terrainOpacity': {
			writeable: true,
			get: function () { return ( terrain !== null ) ? terrain.getOpacity() : 0; },
			set: setTerrainOpacity
		},

		'shadingMode': {
			writeable: true,
			get: function () { return shadingMode; },
			set: function ( x ) { _stateSetter( setShadingMode, 'shadingMode', x ); }
		},

		'surfaceShading': {
			writeable: true,
			get: function () { return surfaceShadingMode; },
			set: function ( x ) { _stateSetter( setSurfaceShadingMode, 'surfaceShading', x ); }
		},

		'cameraType': {
			writeable: true,
			get: function () { return cameraMode; },
			set: function ( x ) { _stateSetter( setCameraMode, 'cameraType', x ); }
		},

		'view': {
			writeable: true,
			get: function () { return VIEW_NONE; },
			set: function ( x ) { _stateSetter( setViewMode, 'view', x ); }
		},

		'cursorHeight': {
			writeable: true,
			get: function () { return cursorHeight; },
			set: setCursorHeight
		},

		'initCursorHeight': {
			writeable: true,
			get: function () { return cursorHeight; },
			set: function ( x ) { cursorHeight = x; }
		},

		'maxHeight': {
			get: function () { return ( limits === null ) ? 0 : limits.max.z; }
		},

		'minHeight': {
			get: function () { return ( limits === null ) ? 0 : limits.min.z; }
		},

		'maxLegLength': {
			get: function () { return stats.maxLegLength; }
		},

		'minLegLength': {
			get: function () { return stats.minLegLength; }
		},

		'section': {
			writeable: true,
			get: function () { return selectedSection; },
			set: function ( x ) { _stateSetter( selectSection, 'section', x ); }
		},

		'sectionByName': {
			writeable: true,
			get: getSelectedSectionName,
			set: setSelectedSectionName
		},

		'highlight': {
			writeable: true,
			set: function ( x ) { _stateSetter( highlightSelection, 'highlight', x ); }
		},

		'routeEdit': {
			writeable: true,
			get: function () { return ( mouseMode === MOUSE_MODE_ROUTE_EDIT ); },
			set: function ( x ) { _setRouteEdit( x ); this.dispatchEvent( { type: 'change', name: 'routeEdit' } ); }
		},

		'setPOI': {
			writeable: true,
			get: function () { return true; },
			set: function ( x ) { _stateSetter( setCameraPOI, 'setPOI', x ); }
		},

		'developerInfo': {
			writeable: true,
			get: function () { return true; },
			set: showDeveloperInfo
		},

		'HUD': {
			writeable: true,
			get: HUD.getVisibility,
			set: HUD.setVisibility
		},

		'cut': {
			writeable: true,
			get: function () { return true; },
			set: cutSection
		},

		'zScale': {
			writeable: true,
			get: function () { return zScale; },
			set: setZScale
		},

		'autoRotate': {
			writeable: true,
			get: function () { return controls.autoRotate; },
			set: function ( x ) { setAutoRotate( !! x ); }
		},

		'autoRotateSpeed': {
			writeable: true,
			get: function () { return controls.autoRotateSpeed / 11; },
			set: function ( x ) { controls.autoRotateSpeed = x * 11; }
		},

		'fullscreen': {
			writeable: true,
			get: isFullscreen,
			set: setFullscreen
		}

	} );

	_enableLayer( FEATURE_BOX, 'box' );

	_conditionalLayer( FEATURE_ENTRANCES, 'entrances' );
	_conditionalLayer( FEATURE_STATIONS,  'stations' );
	_conditionalLayer( FEATURE_TRACES,    'traces' );
	_conditionalLayer( FACE_SCRAPS,       'scraps' );
	_conditionalLayer( FACE_WALLS,        'walls' );
	_conditionalLayer( LEG_SPLAY,         'splays' );
	_conditionalLayer( LEG_SURFACE,       'surfaceLegs' );
	_conditionalLayer( LABEL_STATION,     'stationLabels' );

	Materials.initCache( Viewer );

	HUD.init( container, renderer );

	const progress = HUD.getProgressDial();

	caveLoader = new CaveLoader( caveLoaded, progress.set.bind( progress ) );

	// check if we are defaulting to full screen
	if ( isFullscreen() ) setBrowserFullscreen( true );

	return;

	function _enableLayer ( layerTag, name ) {

		Object.defineProperty( Viewer, name, {
			writeable: true,
			get: function () { return testCameraLayer( layerTag ); },
			set: function ( x ) { setCameraLayer( layerTag, x ); this.dispatchEvent( { type: 'change', name: name } ); }
		} );

	}

	function _conditionalLayer ( layerTag, name ) {

		_enableLayer ( layerTag, name );

		name = 'has' + name.substr( 0, 1 ).toUpperCase() + name.substr( 1 );

		Object.defineProperty( Viewer, name, {
			get: function () { return survey.hasFeature( layerTag ); }
		} );

	}

	function _stateSetter ( modeFunction, name, newMode ) {

		modeFunction( isNaN( newMode ) ? newMode : Number( newMode ) );

		Viewer.dispatchEvent( { type: 'change', name: name } );

	}

	function _setRouteEdit ( x ) {

		mouseMode = x ? MOUSE_MODE_ROUTE_EDIT : MOUSE_MODE_NORMAL;

		switch ( mouseMode ) {

		case MOUSE_MODE_NORMAL:

			mouseTargets = survey.pointTargets;

			break;

		case MOUSE_MODE_ROUTE_EDIT:

			mouseTargets = survey.legTargets;

			break;

		default:

			console.warn( 'invalid mouse mode' );

		}

	}

}

function isFullscreen () {

	return (
		window.innerHeight === container.clientHeight &&
		window.innerWidth === container.clientWidth
	);

}

function setFullscreen ( targetState ) {

	if ( isFullscreen() !== targetState ) {

		container.classList.toggle( 'toggle-fullscreen' );

		setBrowserFullscreen( targetState );

		resize();

	}

}

function setBrowserFullscreen ( targetState ) {

	if ( targetState ) {

		if ( container.webkitRequestFullscreen ) {
			container.webkitRequestFullscreen();
		} else if ( container.mozRequestFullScreen ) {
			container.mozRequestFullScreen();
		} else if ( container.msRequestFullscreen ) {
			container.msRequestFullscreen();
		}

	} else {

		if ( document.webkitExitFullscreen ) {
			document.webkitExitFullscreen();
		} else if ( document.mozCancelFullScreen ) {
			document.mozCancelFullScreen();
		} else if ( document.msExitFullscreen ) {
			document.msExitFullscreen();
		}

	}

}

function setZScale ( scale ) {

	// scale - in range 0 - 1

	const lastScale = Math.pow( 2, ( zScale - 0.5 ) * 4 );
	const newScale  = Math.pow( 2, ( scale - 0.5 )  * 4 );

	survey.applyMatrix( new Matrix4().makeScale( 1, 1, newScale / lastScale ) );

	zScale = scale;

	renderView();

}

function setAutoRotate ( state ) {

	controls.autoRotate = state;

	if ( state ) {

		cameraMove.prepare( null, null );
		cameraMove.start( 2952000 );

	} else {

		cameraMove.stop();

	}

}

function setCursorHeight ( x ) {

	cursorHeight = x;
	Viewer.dispatchEvent( { type: 'cursorChange', name: 'cursorHeight' } );

	renderView();

}

function setTerrainOpacity ( x ) {

	if ( terrain === null ) return;

	terrain.setOpacity( x );
	Viewer.dispatchEvent( { type: 'change', name: 'terrainOpacity' } );

	renderView();

}

function applyTerrainDatumShift( x ) {

	if ( terrain === null ) return;

	terrain.applyDatumShift( x );
	Viewer.dispatchEvent( { type: 'change', name: 'terrainDatumShift' } );

	renderView();

}

function showDeveloperInfo( /* x */ ) {

}

function renderDepthTexture () {

	if ( terrain === null || ! terrain.isLoaded() ) return;

	const dim = 512;

	// set camera frustrum to cover region/survey area

	var width  = container.clientWidth;
	var height = container.clientHeight;

	const range = limits.getSize();

	const scaleX = width / range.x;
	const scaleY = height / range.y;

	if ( scaleX < scaleY ) {

		height = height * scaleX / scaleY;

	} else {

		width = width * scaleY / scaleX;

	}

	// render the terrain to a new canvas square canvas and extract image data

	const rtCamera = new OrthographicCamera( -width / 2, width / 2,  height / 2, -height / 2, -10000, 10000 );

	rtCamera.layers.set( FEATURE_TERRAIN ); // just render the terrain

	scene.overrideMaterial = Materials.getDepthMapMaterial( terrain );

	const renderTarget = new WebGLRenderTarget( dim, dim, { minFilter: LinearFilter, magFilter: NearestFilter, format: RGBAFormat } );

	renderTarget.texture.generateMipmaps = false;
	renderTarget.texture.name = 'CV.DepthMapTexture';

	Materials.setTerrain( terrain );

	renderer.setSize( dim, dim );
	renderer.setPixelRatio( 1 );

	renderer.clear();
	renderer.render( scene, rtCamera, renderTarget, true );

	// correct height between entrances and terrain ( compensates for mismatch beween CRS and datums )

	terrain.addHeightMap( renderer, renderTarget );

	survey.calibrateTerrain( terrain );

	// restore renderer to normal render size and target

	renderer.setRenderTarget();	// revert to screen canvas

	renderer.setSize( container.clientWidth, container.clientHeight );
	renderer.setPixelRatio( window.devicePixelRatio );

	scene.overrideMaterial = null;

	renderView();

	// clear renderList to release objects on heap associated with rtCamera
	renderer.renderLists.dispose();

}

function setCameraMode ( mode ) {

	if ( mode === cameraMode ) return;

	// get offset vector of current camera from target

	const offset = camera.position.clone().sub( controls.target );

	switch ( mode ) {

	case CAMERA_PERSPECTIVE:

		offset.setLength( CAMERA_OFFSET / oCamera.zoom );

		camera = pCamera;

		break;

	case CAMERA_ORTHOGRAPHIC:

		// calculate zoom from ratio of pCamera distance from target to base distance.
		oCamera.zoom = CAMERA_OFFSET / offset.length();

		offset.setLength( CAMERA_OFFSET * 2 );

		camera = oCamera;

		break;

	default:

		console.warn( 'unknown camera mode', mode );
		return;

	}

	// update new camera with position to give same apparent zoom and view

	camera.position.copy( offset.add( controls.target ) );

	camera.updateProjectionMatrix();
	camera.lookAt( controls.target );

	controls.object = camera;

	cameraMode = mode;

	renderView();

}

function initCamera ( camera ) {

	camera.up = upAxis;
	camera.zoom = 1;

	camera.layers.set( 0 );

	camera.layers.enable( LEG_CAVE );
	camera.layers.enable( FEATURE_ENTRANCES );
	camera.layers.enable( FEATURE_BOX );
	camera.layers.enable( FEATURE_SELECTED_BOX );

	camera.position.set( 0, 0, CAMERA_OFFSET );
	camera.lookAt( 0, 0, 0 );
	camera.updateProjectionMatrix();

}

function setCameraLayer ( layerTag, enable ) {

	if ( enable ) {

		oCamera.layers.enable( layerTag );
		pCamera.layers.enable( layerTag );

	} else {

		oCamera.layers.disable( layerTag );
		pCamera.layers.disable( layerTag );

	}

	renderView();

}

function testCameraLayer ( layerTag ) {

	return ( ( camera.layers.mask & 1 << layerTag ) > 0 );

}

function setViewMode ( mode, t ) {

	const cameraPosition = new Vector3();

	switch ( mode ) {

	case VIEW_PLAN:

		// reset camera to start position
		cameraPosition.set( 0, 0, CAMERA_OFFSET );

		break;

	case VIEW_ELEVATION_N:

		cameraPosition.set( 0, CAMERA_OFFSET, 0 );

		break;

	case VIEW_ELEVATION_S:

		cameraPosition.set( 0, -CAMERA_OFFSET, 0 );

		break;

	case VIEW_ELEVATION_E:

		cameraPosition.set( CAMERA_OFFSET, 0, 0 );

		break;

	case VIEW_ELEVATION_W:

		cameraPosition.set( -CAMERA_OFFSET, 0, 0 );

		break;

	default:

		console.warn( 'invalid view mode specified: ', mode );
		return;

	}

	cameraPosition.add( defaultTarget );

	cameraMove.cancel();
	cameraMove.prepare( cameraPosition, defaultTarget );
	cameraMove.start( renderRequired ? t || 240 : 1 );

}

function setTerrainShadingMode ( mode ) {

	if ( terrain === null ) return;
	if ( terrain.setShadingMode( mode, renderView ) ) terrainShadingMode = mode;

	renderView();

}

function setShadingMode ( mode ) {

	if ( terrain === null && ( mode === SHADING_DEPTH || mode === SHADING_DEPTH_CURSOR ) ) return;
	if ( survey.setShadingMode( mode ) ) shadingMode = mode;

	renderView();

}

function setSurfaceShadingMode ( mode ) {

	if ( survey.setLegShading( LEG_SURFACE, mode ) ) surfaceShadingMode = mode;

	renderView();

}

function setTerrainOverlay ( overlayName ) {

	if ( terrain ===  null ) return;

	if ( terrainShadingMode === SHADING_OVERLAY ) {

		activeOverlay = overlayName;

		terrain.setOverlay( overlays[ overlayName ], renderView );

	}

}

function addOverlay ( name, overlayProvider ) {

	overlays[ name ] = new Overlay( overlayProvider, container );

	if ( Object.keys( overlays ).length === 1 ) {

		activeOverlay = name;

	}

}

function addFormatters( stationFormatter ) {

	formatters.station = stationFormatter;

}

function cutSection () {

	if ( selectedSection === 0 ) return;

	survey.remove( terrain );
	survey.cutSection( selectedSection );

	// grab a reference to prevent survey being destroyed in clearView()
	const cutSurvey = survey;

	// reset view
	clearView();

	loadSurvey( cutSurvey, true );

}

function highlightSelection ( id ) {

	survey.highlightSelection( id );

	renderView();

}

function selectSection ( id ) {

	const node = survey.selectSection( id );

	setShadingMode( shadingMode );

	selectedSection = id;

	if ( id === 0 ) {

		const cameraPosition = new Vector3();

		// reset camera to start position
		cameraPosition.set( 0, 0, CAMERA_OFFSET ).add( defaultTarget );

		cameraMove.cancel();
		cameraMove.prepare( cameraPosition, defaultTarget );

		highlightSelection( 0 );

		return;

	}

	if ( node.p === undefined ) {

		if ( node.boundingBox === undefined ) return;
		// a section of the survey rather than a station

		const boundingBox = node.boundingBox.clone();

		cameraMove.prepare( null, boundingBox.applyMatrix4( survey.matrixWorld ) );

	} else {

		// a single station

		cameraMove.prepare( null, survey.getWorldPosition( node.p ) );

	}

	renderView();

}


function getSelectedSectionName () {

	if ( selectedSection === 0 ) {

		return '';

	} else {

		const node = survey.surveyTree.findById( selectedSection );

		return node === undefined ? '' : node.getPath();

	}

}

function setSelectedSectionName ( name ) {

	const id = survey.surveyTree.getIdByPath( name.split( '.' ) );

	selectSection( id === undefined ? 0 : id );

}

function resize () {

	const width  = container.clientWidth;
	const height = container.clientHeight;

	// adjust the renderer to the new canvas size
	renderer.setSize( width, height );

	if ( oCamera === undefined ) return;

	// adjust cameras to new aspect ratio etc.
	oCamera.left   = -width / 2;
	oCamera.right  =  width / 2;
	oCamera.top    =  height / 2;
	oCamera.bottom = -height / 2;

	oCamera.updateProjectionMatrix();

	pCamera.aspect = width / height;

	pCamera.updateProjectionMatrix();

	HUD.resize();

	renderView();

}

function clearView () {

	// clear the current cave model, and clear the screen
	caveIsLoaded = false;

	renderer.clear();

	HUD.setVisibility( false );

	if ( survey ) {

		survey.remove( terrain );
		scene.remove( survey );

	}

	controls.enabled = false;

	survey          = null;
	terrain         = null;
	limits          = null;
	selectedSection = 0;
	mouseMode       = MOUSE_MODE_NORMAL;
	mouseTargets    = [];

	// remove event listeners

	unloadTerrainListeners();

	Materials.flushCache();

	container.removeEventListener( 'mousedown', mouseDown );

	initCamera( pCamera );
	initCamera( oCamera );

}

function loadCave ( file, section ) {

	HUD.getProgressDial().start();

	if ( file instanceof File ) {

		// progressBar.start( 'Loading file ' + file.name + ' ...' );
		caveLoader.loadFile( file );

	} else {

		// progressBar.start( 'Loading file ' + file + ' ...' );
		caveLoader.loadURL( file, section );

	}

}

function caveLoaded ( cave ) {

	HUD.getProgressDial().end();

	if ( ! cave ) {

		alert( 'failed loading cave information' );
		return;

	}

	loadSurvey( new Survey( cave ), false );

}

function setupView () {

	const view = Cfg.value( 'view', {} );

	var name;

	// don't render until all settings made.
	renderRequired = false;

	for ( name in defaultView ) {

		const value = ( view[ name ] !== undefined ) ? view[ name ] : defaultView[ name ];

		// console.log( 'setting view:', name, value );

		Viewer[ name ] = value;

	}

	renderRequired = true;

}

function loadSurvey ( newSurvey ) {

	var syncTerrainLoading = true;
	var firstTiles = true;

	// only render after first SetupView()
	renderRequired = false;

	survey = newSurvey;

	stats = getLegStats( LEG_CAVE );

	setScale( survey );

	terrain = survey.terrain;

	scene.up = upAxis;

	scene.add( survey );
	// scene.add( new DirectionGlobe( survey ) );
	// ClusterLegs( survey );

	selectSection( 0 );

	mouseTargets = survey.pointTargets;

	// set if we have independant terrain maps

	if ( terrain === null ) {

		terrain = new WebTerrain( survey, _terrainReady, _tilesLoaded );
		syncTerrainLoading = false;

	} else {


		survey.add( terrain );

		setTerrainShadingMode( terrainShadingMode );

		renderDepthTexture();

	}

	scene.matrixAutoUpdate = false;

	container.addEventListener( 'mousedown', mouseDown, false );

	// signal any listeners that we have a new cave
	if ( syncTerrainLoading ) Viewer.dispatchEvent( { type: 'newCave', name: 'newCave' } );

	controls.object = camera;
	controls.enabled = true;

	survey.getRoutes().addEventListener( 'changed', _routesChanged );

	setupView();

	caveIsLoaded = true;

	renderView();

	function _terrainReady () {

		if ( terrain.hasCoverage() ) {

			setTerrainShadingMode( terrainShadingMode );

			terrain.tileArea( survey.limits );
			terrain.setDefaultOverlay( overlays[ activeOverlay ] );

			survey.add( terrain );

		} else {

			terrain = null;

			setupView();
			renderView();

			Viewer.dispatchEvent( { type: 'newCave', name: 'newCave' } );

		}

	}

	function _tilesLoaded ( errors ) {

		if ( errors > 0 ) {

			terrain = null;
			console.log( 'errors loading terrain' );

		}

		if ( firstTiles ) {

			// delayed notification to ensure and event listeners get accurate terrain information
			Viewer.dispatchEvent( { type: 'newCave', name: 'newCave' } );

			setupView();

			firstTiles = false;

		}

		renderView();

		if ( terrain !== null ) {

			loadTerrainListeners();

			if ( terrain.depthTexture === null ) renderDepthTexture();

			applyTerrainDatumShift( true );

		}

	}

	function _routesChanged ( /* event */ ) {

		setShadingMode( shadingMode );

	}

}

function loadTerrain ( mode ) {

	if ( terrain !== null && terrain.isLoaded() ) {

		if ( mode ) {

			loadTerrainListeners();

		} else {

			unloadTerrainListeners();

		}

		terrain.setVisibility( mode );

		setCameraLayer( FEATURE_TERRAIN, mode );

		Viewer.dispatchEvent( { type: 'change', name: 'terrain' } );

	}

}

function loadTerrainListeners () {

	clockStart();

	controls.addEventListener( 'end', clockStart );

}

function unloadTerrainListeners () {

	controls.removeEventListener( 'end', clockStart );

	clockStop();

}

function clockStart ( /* event */ ) {

	lastActivityTime = performance.now();

}

function clockStop ( /* event */ ) {

	lastActivityTime = 0;

}

function mouseDown ( event ) {

	var picked, result, i;

	mouse.x =   ( event.clientX / container.clientWidth  ) * 2 - 1;
	mouse.y = - ( event.clientY / container.clientHeight ) * 2 + 1;

	raycaster.setFromCamera( mouse, camera );

	const intersects = raycaster.intersectObjects( mouseTargets, false );
	const l = intersects.length;

	for ( i = 0; i < l; i++ ) {

		picked = intersects[ i ];

		switch ( mouseMode ) {

		case MOUSE_MODE_NORMAL:

			result = _selectStation( picked );

			break;

		case MOUSE_MODE_ROUTE_EDIT:

			result = _selectSegment( picked );

			break;

		}

		if ( result ) break;

	}

	function _selectStation ( picked ) {

		const station = survey.selectStation( picked.index );

		if ( event.button === MOUSE.LEFT ) {

			_showStationPopup( station );

		} else if ( event.button === MOUSE.RIGHT ) {

			_setStationPOI( station );

		}

	}

	function _setStationPOI( station ) {

		if ( station.p === undefined ) return;

		selectSection( station.id );

		cameraMove.start( 60 );
		event.stopPropagation();

		controls.enabled = false;
		container.addEventListener( 'mouseup', _mouseUpLeft );

	}

	function _mouseUpLeft () {

		controls.enabled = true;
		container.removeEventListener( 'mouseup', _mouseUpLeft );

	}

	function _showStationPopup ( station ) {

		const depth = ( terrain ) ? station.p.z - terrain.getHeight( station.p ) : null;

		if ( popup !== null ) return;

		popup = new StationPopup( container, station, survey, depth, formatters.station );

		survey.add( popup );

		container.addEventListener( 'mouseup', _mouseUpRight );

		renderView();

		cameraMove.prepare( null, survey.getWorldPosition( station.p ) );

		return true;

	}

	function _selectSegment ( picked ) {

		const routes = getRoutes();

		routes.toggleSegment( picked.index );

		setShadingMode( SHADING_PATH );

		renderView();

		return true;

	}

	function _mouseUpRight ( /* event */ ) {

		container.removeEventListener( 'mouseup', _mouseUpRight );

		popup.close();
		popup = null;

		survey.clearSelection();

		renderView();

	}

}

var renderView = function () {

	const lPosition = new Vector3();
	const rotation = new Euler();

	return function renderView () {

		if ( ! renderRequired ) return;

		renderer.clear();

		if ( caveIsLoaded ) {

			camera.getWorldRotation( rotation );

			lPosition.copy( lightPosition );

			directionalLight.position.copy( lPosition.applyAxisAngle( upAxis, rotation.z ) );

			survey.update( camera, controls.target );

			renderer.render( scene, camera );

		}

		HUD.renderHUD();

		clockStart();

	};

} ();


function onCameraMoveEnd () {

	Viewer.dispatchEvent( { type: 'moved' } );

	if ( terrain && terrain.isTiled && Viewer.terrain ) setTimeout( updateTerrain, RETILE_TIMEOUT );

}

function updateTerrain () {

	if ( lastActivityTime && performance.now() - lastActivityTime > RETILE_TIMEOUT ) {

		clockStop();

		if ( terrain.zoomCheck( camera ) ) {

			setTimeout( updateTerrain, RETILE_TIMEOUT * 5 );

		}

	}

}

function setCameraPOI () {

	cameraMove.start( 100 );

}

function setScale ( obj ) {

	const width  = container.clientWidth;
	const height = container.clientHeight;

	// scaling to compensate distortion introduced by projection ( x and y coords only ) - approx only
	const scaleFactor = survey.scaleFactor;

	limits = survey.limits;

	const range = limits.getSize();

	// initialize cursor height to be mid range of heights
	cursorHeight = 0;

	// initialize vertical scaling to none
	zScale = 0.5;

	const hScale = Math.min( width / range.x, height / range.y );
	const vScale = hScale * scaleFactor;

	const scale = new Vector3( hScale, hScale, vScale );

	obj.scale.copy( scale );

	obj.position.copy( survey.modelLimits.getCenter().multiply( scale ).negate() );

	HUD.setScale( vScale );

	// pass to survey to adjust size of symbology

	obj.setScale( vScale );

}

function getLegStats ( type ) {

	return survey.getFeature( type ).stats;

}

function getControls () {

	return controls;

}

function getMetadata () {

	return survey.getMetadataURL();

}

function getRoutes () {

	return survey.getRoutes();

}

function getSurveyTree () {

	return survey.surveyTree;

}

// export public interface

Object.assign( Viewer, {
	init:          init,
	clearView:     clearView,
	loadCave:      loadCave,
	getMetadata:   getMetadata,
	getRoutes:     getRoutes,
	getLegStats:   getLegStats,
	getSurveyTree: getSurveyTree,
	getControls:   getControls,
	renderView:    renderView,
	addOverlay:    addOverlay,
	addFormatters: addFormatters
} );

export { Viewer };


// EOF