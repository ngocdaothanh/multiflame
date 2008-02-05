package net.web20games.container {
	import flash.events.EventDispatcher;
	import flash.net.XMLSocket;
	import flash.utils.Timer;
	import flash.events.*;
	import com.adobe.serialization.json.JSON;
	
	import net.web20games.container.events.TransporterEvent;

	public class Transporter extends EventDispatcher {
		private var _socket:XMLSocket;
		private var _host:String;
		private var _port:int;
		private var _pendingInvocation:Array;
		private var _lastInvocation:Array;
		private var _redirecting:Boolean;

		public function Transporter(host:String, port:int):void {
			_socket = new XMLSocket();
			_socket.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecurityError);
			_socket.addEventListener(IOErrorEvent.IO_ERROR, onIOError);
			_socket.addEventListener(Event.CONNECT, onConnect);
			_socket.addEventListener(DataEvent.DATA, onData);
			_socket.addEventListener(Event.CLOSE, onClose);
			_host = host;
			_port = port;
			_socket.connect(host, port);
		}

		public function redirect(host:String, port:int):void {
			_host = host;
			_port = port;
			_redirecting = true;
			_socket.close();
		}

		public function invoke(cmd:int, arg:Object) {
			_lastInvocation = [cmd, arg];
			if (_socket.connected) {
				_socket.send(JSON.encode(_lastInvocation));
			} else {
				_pendingInvocation = _lastInvocation;
				_socket.connect(_host, _port);
			}
		}

		private function onSecurityError(event:SecurityErrorEvent):void {
			dispatchEvent(new TransporterEvent(TransporterEvent.CONNECTION_ERROR, null));
		}

		private function onIOError(event:IOErrorEvent):void {
			dispatchEvent(new TransporterEvent(TransporterEvent.CONNECTION_ERROR, null));
		}

		private function onConnect(event:Event):void {
			if (_pendingInvocation != null) {
				var cmd:int    = _pendingInvocation[0];
				var arg:Object = _pendingInvocation[1];
				invoke(cmd, arg);
				_pendingInvocation = null;
			}
		}

		private function onData(event:DataEvent):void {
			var a:Array = JSON.decode(event.data);
			var cmd:int    = a[0];
			var arg:Object = a[1];
			dispatchEvent(new TransporterEvent(cmd, arg));
		}

		private function onClose(event:Event):void {
			if (_redirecting) {
				_redirecting = false;
				_pendingInvocation = _lastInvocation;
				_socket.connect(_host, _port);
			} else {
				dispatchEvent(new TransporterEvent(TransporterEvent.CONNECTION_CLOSE, null));
			}
		}
	}
}