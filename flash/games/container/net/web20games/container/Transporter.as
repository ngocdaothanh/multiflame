package net.web20games.container {
	import flash.utils.*;
	import flash.events.*;

	import revent.Client;
	import revent.CallEvent;
	import net.web20games.container.events.TransporterEvent;

	public class Transporter extends EventDispatcher {
		private var _client:Client;
		private var _host:String;
		private var _port:int;
		private var _pendingInvocation:Array;
		private var _lastInvocation:Array;
		private var _redirecting:Boolean;

		public function Transporter(host:String, port:int):void {
			_client = new Client();
			_client.addEventListener(CallEvent.SECURITY_ERROR, onSecurityError);
			_client.addEventListener(CallEvent.IO_ERROR, onIOError);
			_client.addEventListener(CallEvent.CONNECT, onConnect);
			_client.addEventListener(CallEvent.CALL, onCall);
			_client.addEventListener(CallEvent.RESULT, onResult);
			_client.addEventListener(CallEvent.CLOSE, onClose);

			_host = host;
			_port = port;
			_redirecting = false;
			_client.connect(host, port);
		}

		public function redirect(host:String, port:int):void {
			_host = host;
			_port = port;
			_redirecting = true;
			_pendingInvocation = _lastInvocation;
			_client.close();
			setTimeout(reconnectOnRedirect, 1000);
		}

		public function call(cmd:int, arg:Object) {
			_lastInvocation = [cmd, arg];
			if (_client.connected) {
				_client.call(_lastInvocation[0], _lastInvocation[1]);
			} else {
				_pendingInvocation = _lastInvocation;
				_client.connect(_host, _port);
			}
		}

		private function onSecurityError(event:SecurityErrorEvent):void {
			dispatchEvent(new TransporterEvent(TransporterEvent.CONNECTION_ERROR, null));
		}

		private function onIOError(event:IOErrorEvent):void {
			dispatchEvent(new TransporterEvent(TransporterEvent.CONNECTION_ERROR, null));
		}

		private function onConnect(event:CallEvent):void {
			if (_pendingInvocation != null) {
				var cmd:int    = _pendingInvocation[0];
				var arg:Object = _pendingInvocation[1];
				_pendingInvocation = null;
				call(cmd, arg);
			}
		}

		private function onCall(event:CallEvent):void {
			dispatchEvent(new TransporterEvent(event.cmd as int, event.value));
		}

		private function onResult(event:CallEvent):void {
			dispatchEvent(new TransporterEvent(event.cmd as int, event.value));
		}

		private function onClose(event:CallEvent):void {
			if (!_redirecting) {
				dispatchEvent(new TransporterEvent(TransporterEvent.CONNECTION_CLOSE, null));
			}
		}

		private function reconnectOnRedirect() {
			_redirecting = false;
			_client.connect(_host, _port);
		}
	}
}