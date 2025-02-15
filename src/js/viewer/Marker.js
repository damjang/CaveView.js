import { Point } from './Point';

class Marker extends Point {

	isMarker = true;

	constructor ( ctx, count ) {

		const materials = ctx.materials;

		super( materials.getClusterMaterial( count ), ctx );
		this.renderOrder = 1;

	}

	adjustHeight ( func ) {

		this.position.setZ( func( this.position ) + 10 );

	}

}

export { Marker };