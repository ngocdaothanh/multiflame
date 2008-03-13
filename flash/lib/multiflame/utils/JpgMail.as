package multiflame.utils {
	import flash.display.*;
	import flash.events.*;
	import flash.utils.ByteArray;

	import com.adobe.images.JPGEncoder;

	import revent.Client;
	import revent.CallEvent;

	/**
	 * This class sends mail to Asonr with an optional JPEG image and dispatches
	 * Event.COMPLETE when done.
	 */
	public class JpgMail extends EventDispatcher {
		public var img:ByteArray;

		private var _client:Client;
		private var _host:String;
		private var _port:int;
		private var _cmdMail:int;

		private var _mailObj:Object;

		public function JpgMail(host:String, port:int, cmdMail:int):void {
			_client = new Client();
			_host = host;
			_port = port;
			_cmdMail = cmdMail;
			_client.addEventListener(CallEvent.SECURITY_ERROR, onSecurityError);
			_client.addEventListener(CallEvent.CONNECT, onConnect);
			_client.addEventListener(CallEvent.CLOSE, onClose);
			_client.addEventListener(CallEvent.IO_ERROR, onIOError);
			_client.addEventListener(CallEvent.RESULT, onResult);
		}

		public function send(encryptedCode:ByteArray, code:String,
				recipient:String, subject:String, body:String, img:BitmapData):void {

			var enc:JPGEncoder = new JPGEncoder(60);
			_mailObj = {
				encryptedCode: encryptedCode,
				code:          code,
				recipient:     recipient,
				subject:       subject,
				body:          body,
				jpg:           enc.encode(img)
			};

			_client.connect(_host, _port);
		}

		//----------------------------------------------------------------------------

		private function onSecurityError(event:CallEvent):void {
			trace("onSecurityError");
		}
		
		private function onConnect(event:CallEvent):void {
			_client.call(_cmdMail, _mailObj);
		}
		
		private function onClose(event:CallEvent):void {
			dispatchEvent(new Event(Event.COMPLETE));
		}
		
		private function onIOError(event:CallEvent):void {
			trace("onIOError");
		}
		
		private function onResult(event:CallEvent):void {
		}

		private function onLoaderIOError(event:IOErrorEvent):void {
			trace("onLoaderIOError");
		}
	}
}
