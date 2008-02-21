package {
	import flash.display.Sprite;
	import flash.display.BitmapData;
	import flash.text.TextField;
	import flash.events.Event;
	import flash.events.StatusEvent;
	import flash.media.Camera;
	import flash.media.Video;
	import flash.geom.Rectangle;
	import flash.utils.ByteArray;

	public class Document extends Sprite {
		private static const THRESHOLD:int = 320*240*10;

		private var _vid:Video;
		private var _bmd:BitmapData;

		public function Document():void {
			_status.text = "Please connect a webcam.";
			if (Camera.names.length == 0) {
				return;
			}

			var cam:Camera = Camera.getCamera();
			if (cam == null) {
				return;
			}

			_vid = new Video();
			_vid.smoothing = true;
			cam.addEventListener(StatusEvent.STATUS, statusHandler);
			_status.text = "Please allow access to your webcam.";
			_vid.attachCamera(cam);
			addChild(_vid);

			_bmd = new BitmapData(_vid.width, _vid.height);
		}

		private function statusHandler(event:StatusEvent):void {
			if (event.code == "Camera.Unmuted") {
				addEventListener(Event.ENTER_FRAME, onEnterFrame);
			}
		}

		private function onEnterFrame(event:Event):void {
			var bmd:BitmapData = new BitmapData(_vid.width, _vid.height);
			bmd.draw(_vid);
			if (_bmd == null) {
				_bmd = bmd;
				return;
			}

			var sum:int = 0;
			var p1, p2:uint;
			var r, g, b:int;
			var b1, b2:int;
			for (var row:int = 0; row < _vid.height; row++) {
				for (var col:int = 0; col < _vid.width; col++) {
					p2 = bmd.getPixel(row, col);
					p1 = _bmd.getPixel(row, col);

					r = (p2 >> 8*2) & 0x0000FF;
					g = (p2 >> 8) & 0x0000FF;
					b = p2 & 0x0000FF;
					b2 = 0.3*r + 0.3*g + 0.3*b;

					r = (p1 >> 8*2) & 0x0000FF;
					g = (p1 >> 8) & 0x0000FF;
					b = p1 & 0x0000FF;
					b1 = 0.3*r + 0.3*g + 0.3*b;

					sum += Math.abs(b2 - b1);
				}
			}
			//trace(sum);
			if (sum > THRESHOLD) {
				_status.text = "Moved";
			} else {
				_status.text = "";
			}

			_bmd = bmd;
		}
	}
}