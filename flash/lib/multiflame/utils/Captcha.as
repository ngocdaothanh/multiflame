package multiflame.utils {
	import flash.net.Socket;
	import flash.display.Loader;
	import flash.display.DisplayObject;
	import flash.events.*;
	import flash.utils.ByteArray;

	import revent.Client;
	import revent.CallEvent;

	/**
	 * This class gets CAPTCHA from Asonr and dispatch Event.COMPLETE when done.
	 */
	public class Captcha extends EventDispatcher {
		public static const CAPTCHA:String = "CAPTCHA";

		public var encryptedCode:ByteArray;
		public var img:DisplayObject;

		private var _client:Client;
		private var _host:String;
		private var _port:int;
		private var _cmdCaptcha:int;

		public function Captcha(host:String, port:int, cmdCaptcha:int):void {
			_client = new Client();
			_host = host;
			_port = port;
			_cmdCaptcha = cmdCaptcha;
			_client.addEventListener(CallEvent.SECURITY_ERROR, onSecurityError);
			_client.addEventListener(CallEvent.CONNECT, onConnect);
			_client.addEventListener(CallEvent.CLOSE, onClose);
			_client.addEventListener(CallEvent.IO_ERROR, onIOError);
			_client.addEventListener(CallEvent.RESULT, onResult);
		}

		public function receive():void {
			_client.connect(_host, _port);
		}

		//----------------------------------------------------------------------------

		private function onSecurityError(event:CallEvent):void {
			trace("onSecurityError");
		}
		
		private function onConnect(event:CallEvent):void {
			_client.call(_cmdCaptcha, null);
		}
		
		private function onClose(event:CallEvent):void {
			trace("onClose");
		}
		
		private function onIOError(event:CallEvent):void {
			trace("onIOError");
		}
		
		private function onResult(event:CallEvent):void {
			encryptedCode = event.value[0];
			var ba:ByteArray = event.value[1];

			var loader:Loader = new Loader();
			loader.contentLoaderInfo.addEventListener(Event.INIT, onCaptchaLoaded);
			loader.contentLoaderInfo.addEventListener(IOErrorEvent.IO_ERROR, onLoaderIOError);
			loader.loadBytes(ba);
		}

		private function onCaptchaLoaded(event:Event):void {
			img = event.target.content as DisplayObject;
			dispatchEvent(new Event(Event.COMPLETE));
		}

		private function onLoaderIOError(event:IOErrorEvent):void {
			trace("onLoaderIOError");
		}
	}
}