import { Control } from './Control';
import { Vector2 } from '../Three';

class CompassControl extends Control {

	constructor ( hudObject, viewer ) {

		const dim = hudObject.stdWidth * 2;

		super( viewer.container, dim, dim, handleEnter );

		const controls = viewer.getControls();

		const point = new Vector2();
		const center = new Vector2();

		let dragging = false;
		let dragged = false;

		let startAngle = 0;

		this.positionHitRegion( hudObject.stdMargin, hudObject.stdMargin );

		const handlers = {
			pointerleave: handleLeave,
			pointermove:  handlePointerMove,
			pointerdown:  handlePointerDown,
			pointerup:    handlePointerUp
		};

		const self = this;

		function handleEnter ( event ) {

			if ( ! viewer.HUD ) return;

			self.commonEnter( event.currentTarget, handlers );

			const bc = self.rect;

			center.set( bc.left + hudObject.stdWidth, bc.top + hudObject.stdWidth );
			dragging = false;

		}

		function handleLeave ( event ) {

			if ( dragging ) controls.end();

			self.commonLeave( event.currentTarget, handlers );

		}

		function handlePointerDown ( event ) {

			event.stopPropagation();

			dragging = true;
			dragged = false;

			point.set( event.clientX, event.clientY ).sub( center );
			startAngle = point.angle();

		}

		function handlePointerUp ( event ) {

			event.stopPropagation();

			if ( dragged ) {

				controls.end();

			} else {

				handleClick();

			}

			dragging = false;

		}

		function handleClick () {

			// select cardinal point from quadrant of control clicked on

			if ( point.x > point.y ) {

				if ( point.x < -point.y ) {

					viewer.azimuthAngle = 0;

				} else {

					viewer.azimuthAngle = Math.PI / 2;

				}

			} else {

				if ( point.x > -point.y ) {

					viewer.azimuthAngle = Math.PI;

				} else {

					viewer.azimuthAngle = 3 * Math.PI / 2;

				}

			}

		}

		function handlePointerMove ( event ) {

			event.stopPropagation();
			event.preventDefault();

			if ( ! dragging ) return;

			point.set( event.clientX, event.clientY ).sub( center );

			const angle = point.angle();

			controls.rotateLeft( startAngle - angle );

			startAngle = angle;
			dragged = true;

		}

	}

}

export { CompassControl };