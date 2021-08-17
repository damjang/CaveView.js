import {
	Box3,
	Float32BufferAttribute,
	InstancedBufferGeometry,
	InstancedInterleavedBuffer,
	InterleavedBufferAttribute,
	InstancedBufferAttribute,
	Sphere,
	Vector3,
} from '../three';

class LineSegmentsGeometry extends InstancedBufferGeometry {

	constructor () {

		super();

		this.type = 'LineSegmentsGeometry';

		const positions = [ - 1, 2, 0, 1, 2, 0, - 1, 1, 0, 1, 1, 0, - 1, 0, 0, 1, 0, 0, - 1, - 1, 0, 1, - 1, 0 ];
		const uvs = [ - 1, 2, 1, 2, - 1, 1, 1, 1, - 1, - 1, 1, - 1, - 1, - 2, 1, - 2 ];
		const index = [ 0, 2, 1, 2, 3, 1, 2, 4, 3, 4, 5, 3, 4, 6, 5, 6, 7, 5 ];

		this.setIndex( index );
		this.setAttribute( 'position', new Float32BufferAttribute( positions, 3 ) );
		this.setAttribute( 'uv', new Float32BufferAttribute( uvs, 2 ) );

	}

}

Object.assign( LineSegmentsGeometry.prototype, {

	isLineSegmentsGeometry: true,

	applyMatrix4: function ( matrix ) {

		const start = this.attributes.instanceStart;
		const end = this.attributes.instanceEnd;

		if ( start !== undefined ) {

			start.applyMatrix4( matrix );

			end.applyMatrix4( matrix );

			start.needsUpdate = true;

		}

		if ( this.boundingBox !== null ) {

			this.computeBoundingBox();

		}

		if ( this.boundingSphere !== null ) {

			this.computeBoundingSphere();

		}

		return this;

	},

	setPositions: function ( array ) {

		let lineSegments;

		if ( array instanceof Float32Array ) {

			lineSegments = array;

		} else if ( Array.isArray( array ) ) {

			lineSegments = new Float32Array( array );

		}

		const instanceBuffer = new InstancedInterleavedBuffer( lineSegments, 6, 1 ); // xyz, xyz

		this.setAttribute( 'instanceStart', new InterleavedBufferAttribute( instanceBuffer, 3, 0 ) ); // xyz
		this.setAttribute( 'instanceEnd', new InterleavedBufferAttribute( instanceBuffer, 3, 3 ) ); // xyz

		//

		this.computeBoundingBox();
		this.computeBoundingSphere();

		return this;

	},

	setColors: function ( array ) {

		let colors;

		if ( array instanceof Float32Array ) {

			colors = array;

		} else if ( Array.isArray( array ) ) {

			colors = new Float32Array( array );

		}

		const instanceColorBuffer = new InstancedInterleavedBuffer( colors, 6, 1 ); // rgb, rgb

		this.setAttribute( 'instanceColorStart', new InterleavedBufferAttribute( instanceColorBuffer, 3, 0 ) ); // rgb
		this.setAttribute( 'instanceColorEnd', new InterleavedBufferAttribute( instanceColorBuffer, 3, 3 ) ); // rgb

		return this;

	},

	setHide: function ( array ) {

		let hiddenVertices;

		if ( array instanceof Float32Array ) {

			hiddenVertices = array;

		} else if ( Array.isArray( array ) ) {

			hiddenVertices = new Float32Array( array );

		}

		this.setAttribute( 'instanceHideVertex', new InstancedBufferAttribute( hiddenVertices, 1, false, 1 ) );

		return this;

	},

	clearHide: function () {

		this.deleteAttribute( 'instanceHideVertex' );

	},

	computeBoundingBox: function () {

		const box = new Box3();

		return function computeBoundingBox() {

			if ( this.boundingBox === null ) {

				this.boundingBox = new Box3();

			}

			const start = this.attributes.instanceStart;
			const end = this.attributes.instanceEnd;

			if ( start !== undefined && end !== undefined ) {

				this.boundingBox.setFromBufferAttribute( start );

				box.setFromBufferAttribute( end );

				this.boundingBox.union( box );

			}

		};

	}(),

	computeBoundingSphere: function () {

		const vector = new Vector3();

		return function computeBoundingSphere() {

			if ( this.boundingSphere === null ) {

				this.boundingSphere = new Sphere();

			}

			if ( this.boundingBox === null ) {

				this.computeBoundingBox();

			}

			const start = this.attributes.instanceStart;
			const end = this.attributes.instanceEnd;

			if ( start !== undefined && end !== undefined ) {

				const center = this.boundingSphere.center;

				this.boundingBox.getCenter( center );

				let maxRadiusSq = 0;

				for ( let i = 0, il = start.count; i < il; i ++ ) {

					vector.fromBufferAttribute( start, i );
					maxRadiusSq = Math.max( maxRadiusSq, center.distanceToSquared( vector ) );

					vector.fromBufferAttribute( end, i );
					maxRadiusSq = Math.max( maxRadiusSq, center.distanceToSquared( vector ) );

				}

				this.boundingSphere.radius = Math.sqrt( maxRadiusSq );

				if ( isNaN( this.boundingSphere.radius ) ) {

					console.error( 'THREE.LineSegmentsGeometry.computeBoundingSphere(): Computed radius is NaN. The instanced position data is likely to have NaN values.', this );

				}

			}

		};

	}(),

} );

export { LineSegmentsGeometry };
