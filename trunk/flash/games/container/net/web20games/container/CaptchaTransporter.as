package net.web20games.container {
	import flash.net.Socket;
	import flash.display.Loader;
	import flash.display.DisplayObject;
	import flash.events.*;
	import flash.utils.ByteArray;

	public class CaptchaTransporter extends EventDispatcher {
		public static const CAPTCHA:String = "CAPTCHA";

		private static const CAPTCHA_ENCRYPTED_CODE_LENGTH_DIGITS:int = 2;
		private static const CAPTCHA_IMG_SIZE_DIGITS:int              = 5;

		public var encryptedCode:String;
		public var img:DisplayObject;
		
		private var _socket:Socket;
		private var _codeLen:int;
		private var _imgLen:int;

		public function CaptchaTransporter():void {
			_socket = new Socket();
			_socket.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecurityError);
			_socket.addEventListener(Event.CONNECT, onConnect);
			_socket.addEventListener(Event.CLOSE, onClose);
			_socket.addEventListener(IOErrorEvent.IO_ERROR, onIOError);
			_socket.addEventListener(ProgressEvent.SOCKET_DATA , onData);

			_codeLen = 0;
			_imgLen = 0;
		}

		public function getCaptcha():void {
			_socket.connect(Channel.instance.host, 443);
		}

		private function onSecurityError(event:SecurityErrorEvent):void {
			trace("onSecurityError");
		}
		
		private function onConnect(event:Event):void {
			_socket.writeUTFBytes(Channel.CMD_CAPTCHA);
			_socket.writeByte(0);
			_socket.flush();
		}
		
		private function onClose(event:Event):void {
			trace("onClose");
		}
		
		private function onIOError(event:IOErrorEvent):void {
			trace("onIOError");
		}
		
		private function onData(event:ProgressEvent):void {
			if (_codeLen == 0 && _imgLen == 0) {
				// Header
				if (_socket.bytesAvailable < CAPTCHA_ENCRYPTED_CODE_LENGTH_DIGITS + CAPTCHA_IMG_SIZE_DIGITS) {
					return;
				}
				_codeLen = int(_socket.readUTFBytes(CAPTCHA_ENCRYPTED_CODE_LENGTH_DIGITS));
				_imgLen = int(_socket.readUTFBytes(CAPTCHA_IMG_SIZE_DIGITS));
			}
			
			// Data
			if (_socket.bytesAvailable < _codeLen + _imgLen) {
				return;
			}
			encryptedCode = _socket.readUTFBytes(_codeLen);
			var loader:Loader = new Loader();
			loader.contentLoaderInfo.addEventListener(Event.INIT, onCaptchaLoaded);
			var ba:ByteArray = new ByteArray();
			_socket.readBytes(ba, 0, _imgLen);
			loader.loadBytes(ba);

			// Reset
			_codeLen = 0;
			_imgLen = 0;
			if (ba.bytesAvailable > _imgLen) {
				onData(null);
			}
		}
		
		private function onCaptchaLoaded(event:Event):void {
			img = event.target.content as DisplayObject;
			dispatchEvent(new Event(CAPTCHA));
		}
	}
}