#include <fog_pars_vertex>

// glyph shader, each instance represents one glyph.

uniform float cellScale;
uniform vec2 scale;
uniform mat2 rotate;
uniform vec2 viewPort;

attribute float offsets;

attribute vec2 instanceUvs;
attribute float instanceOffsets;
attribute float instanceWidths;

varying vec2 vUv;

void main() {

	// select glyph from atlas ( with proportional spacing ).

	vUv = instanceUvs + vec2( position.x * cellScale * instanceWidths, position.y * cellScale );

	// scale by glyph width ( vertices form unit square with (0,0) origin )

	vec2 newPosition = vec2( position.x * instanceWidths, position.y );

	// move to correct offset in string

	newPosition.x += instanceOffsets;
	newPosition.y += offsets;

	// rotate as required

	newPosition = rotate * newPosition;

	// position of GlyphString object on screen

	vec4 offset = projectionMatrix * modelViewMatrix * vec4( 0.0, 0.0, 0.0, 1.0 );

	// scale glyphs
	newPosition *= scale;

	// move to clip space

	newPosition.xy *= offset.w;

	vec4 mvPosition = modelViewMatrix * vec4( position, 1.0 );

	gl_Position = vec4( newPosition, 0.0, 0.0 ) + offset;

	vec2 snap = viewPort / gl_Position.w;

	gl_Position.xy =  ( trunc( gl_Position.xy * snap ) + 0.5 ) / snap;

	#include <fog_vertex>

}