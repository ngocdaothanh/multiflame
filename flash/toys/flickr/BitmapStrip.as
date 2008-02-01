package {
	import flash.display.*;
	import flash.events.*;

	public class BitmapStrip extends EventDispatcher {
		public static const DOWNLOAD_LAST_OK:String = "BITMAP_STRIP_DOWNLOAD_LAST_OK";
		public static const DOWNLOAD_FIRST_OK:String = "BITMAP_STRIP_DOWNLOAD_FIRST_OK";

		private static const BUFFER_SIZE:int = 7;  // Minimum is 7

		private var _bitmapURLs:Array;
		private var _width:int;
		private var _height:int;

		private var _buffer:Array;
		private var _iBuffer:int;
		private var _iAll:int;

		private var _downloadingIndices:Array;
		private var _removedBuffer:Array;

		public function BitmapStrip(bitmapURLs:Array, width:int, height:int):void {
			_bitmapURLs = bitmapURLs;
			_width = width;
			_height = height;

			_buffer = new Array();
			_iBuffer = 0;
			_iAll = 0;

			_downloadingIndices = new Array();
			_removedBuffer = new Array();
		}

		public function download():void {
			var imageURL:BitmapURL = _bitmapURLs[0];
			imageURL.addEventListener(BitmapURL.DOWNLOAD_BUFFER_DONE, onDownloadBufferDone);
			imageURL.download(BitmapURL.DOWNLOAD_BUFFER_DONE);
		}

		public function get buffer():Array {
			return _buffer;
		}

		public function get removedBuffer():Array {
			return _removedBuffer;
		}

		public function navigate(forward:Boolean):void {
			// Do not navigate too far
			if (!forward && _iAll == 0 && _buffer[0].x > _width/2) {
				return;
			}
			if (forward && (BUFFER_SIZE >= _bitmapURLs.length || _iAll == _bitmapURLs.length - 1) &&
					(_buffer[_buffer.length - 1].x + _buffer[_buffer.length - 1].width)< _width/2) {
				return;
			}

			var dx:int = (forward)? -1 : 1;
			for (var i:int = 0; i < _buffer.length; i++) {
				_buffer[i].x += dx;
			}

			if (BUFFER_SIZE >= _bitmapURLs.length || _buffer.length < BUFFER_SIZE) {
				return;
			}

			if (dx < 0 && _buffer[_iBuffer].x < 0) {
				_iBuffer++;
				_iAll++;
				if (_iBuffer > BUFFER_SIZE - 1) {
					_iBuffer = BUFFER_SIZE - 1;
				}
				if (_iAll > _bitmapURLs.length - 1) {
					_iAll = _bitmapURLs.length - 1;
				}
			} else if (dx > 0 && _buffer[_iBuffer].x > _width) {
				_iBuffer--;
				_iAll--;
				if (_iBuffer < 0) {
					_iBuffer = 0;
				}
				if (_iAll < 0) {
					_iAll = 0;
				}
			}

			var c:int = int(BUFFER_SIZE/2);
			var index:int;
			if (_iBuffer > c && (_iAll + c) < _bitmapURLs.length) {
				index = _iAll + c;
				if (_downloadingIndices.indexOf(index) != -1) {
					return;
				}
				_downloadingIndices.push(index);
				_bitmapURLs[index].addEventListener(BitmapURL.DOWNLOAD_LAST_DONE, onDownloadLastDone);
				_bitmapURLs[index].download(BitmapURL.DOWNLOAD_LAST_DONE);
			} else if (_iBuffer < c && (_iAll - c) >= 0) {
				index = _iAll - c;
				if (_downloadingIndices.indexOf(index) != -1) {
					return;
				}
				_downloadingIndices.push(index);
				_bitmapURLs[index].addEventListener(BitmapURL.DOWNLOAD_FIRST_DONE, onDownloadFirstDone);
				_bitmapURLs[index].download(BitmapURL.DOWNLOAD_FIRST_DONE);
			}
		}

		// --------------------------------------------------------------------------

		private function onDownloadBufferDone(event:Event):void {
			var bitmap:Bitmap = event.target.bitmap;
			if (bitmap != null) {
				if (_buffer.length > 0) {
					bitmap.x = _buffer[_buffer.length - 1].x + _buffer[_buffer.length - 1].width;
				}
				bitmap.y = (_height - bitmap.height)/2;
				_buffer.push(bitmap);

				var done:Event = new Event(DOWNLOAD_LAST_OK);
				dispatchEvent(done);
			} else {
				_bitmapURLs.splice(_buffer.length, 1);
			}

			if (_buffer.length < BUFFER_SIZE && _buffer.length < _bitmapURLs.length) {
				var photo:BitmapURL = _bitmapURLs[_buffer.length];
				photo.addEventListener(BitmapURL.DOWNLOAD_BUFFER_DONE, onDownloadBufferDone);
				photo.download(BitmapURL.DOWNLOAD_BUFFER_DONE);
			}
		}

		private function onDownloadLastDone(event:Event):void {
			var bitmap:Bitmap = Bitmap(event.target.bitmap);
			if (bitmap != null) {
				bitmap.x = _buffer[_buffer.length - 1].x + _buffer[_buffer.length - 1].width;
				bitmap.y = (_height - bitmap.height)/2;
				_buffer.push(bitmap);
				if (_removedBuffer == null) {
					_removedBuffer = new Array();
				}
				_removedBuffer.push(_buffer.shift());
				_iBuffer--;

				for (var i:int = 0; i < _downloadingIndices.length; i++) {
					if (event.target == _bitmapURLs[_downloadingIndices[i]]) {
						_downloadingIndices.splice(i, 1);
						break;
					}
				}

				var done:Event = new Event(DOWNLOAD_LAST_OK);
				dispatchEvent(done);
			}
		}

		private function onDownloadFirstDone(event:Event):void {
			var bitmap:Bitmap = Bitmap(event.target.bitmap);
			if (bitmap != null) {
				bitmap.x = _buffer[0].x - bitmap.width;
				bitmap.y = (_height - bitmap.height)/2;
				_buffer.unshift(bitmap);
				_removedBuffer.push(_buffer.pop());
				_iBuffer++;

				for (var i:int = 0; i < _downloadingIndices.length; i++) {
					if (event.target == _bitmapURLs[_downloadingIndices[i]]) {
						_downloadingIndices.splice(i, 1);
						break;
					}
				}

				var done:Event = new Event(DOWNLOAD_FIRST_OK);
				dispatchEvent(done);
			}
		}
	}
}