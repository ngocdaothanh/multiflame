package net.web20games.container {
	import flash.net.Socket;
	import flash.display.Loader;
	import flash.display.DisplayObject;
	import flash.events.*;
	import flash.utils.ByteArray;

	import revent.Client;
	import revent.CallEvent;

	public class CaptchaTransporter extends EventDispatcher {
		public static const CAPTCHA:String = "CAPTCHA";

		private var _client:Client;
		public var encryptedCode:String;
		public var img:DisplayObject;

		public function CaptchaTransporter():void {
			_client = new Client();
			_client.addEventListener(CallEvent.SECURITY_ERROR, onSecurityError);
			_client.addEventListener(CallEvent.CONNECT, onConnect);
			_client.addEventListener(CallEvent.CLOSE, onClose);
			_client.addEventListener(CallEvent.IO_ERROR, onIOError);
			_client.addEventListener(CallEvent.RESULT, onResult);
		}

		public function getCaptcha():void {
			_client.connect(Channel.instance.host, Channel.instance.port);
		}

		private function onSecurityError(event:CallEvent):void {
			trace("onSecurityError");
		}
		
		private function onConnect(event:CallEvent):void {
			_client.call(Channel.CMD_CAPTCHA, null);
		}
		
		private function onClose(event:CallEvent):void {
			trace("onClose");
		}
		
		private function onIOError(event:CallEvent):void {
			trace("onIOError");
		}
		
		private function onResult(event:CallEvent):void {
			encryptedCode = event.value[0];
			var ba:ByteArray = Client.bytesToByteArray(event.value[1]);

			var loader:Loader = new Loader();
			loader.contentLoaderInfo.addEventListener(Event.INIT, onCaptchaLoaded);
			loader.contentLoaderInfo.addEventListener(IOErrorEvent.IO_ERROR, onLoaderIOError);
			loader.loadBytes(ba);
		}

		private function onCaptchaLoaded(event:Event):void {
			img = event.target.content as DisplayObject;
			dispatchEvent(new Event(CAPTCHA));
		}

		private function onLoaderIOError(event:IOErrorEvent):void {
			trace("onLoaderIOError");
		}
	}
}