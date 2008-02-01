/*
Alessandro Crugnola
alessandro@sephiroth.it
http://www.sephiroth.it
*/
package com.adobe.webapis.gettext {
	import flash.net.*;
	import flash.utils.*;
	import flash.errors.*;
	import flash.events.*;

	public class GetText extends EventDispatcher {
		public static const COMPLETE:String = "complete";
		public static const IO_ERROR:String = "ioError";
		public static const ERROR:String    = "error";

		private var translations:Object;
		private var charset:     String;
		private var info:        Object;

		/**
		 * Install the gettext support for the specified application
		 * @param url URL to the mo-file
		 */
		public final function install(url):void {
			var xstream:URLStream = new URLStream();
			xstream.addEventListener(Event.COMPLETE,         this.handleEvent);
			xstream.addEventListener(Event.OPEN,             this.handleEvent);
			xstream.addEventListener(ProgressEvent.PROGRESS, this.handleEvent);
			xstream.addEventListener(HTTPStatusEvent.HTTP_STATUS,       this.handleEvent);
			xstream.addEventListener(IOErrorEvent.IO_ERROR,             this.handleEvent);
			xstream.addEventListener(SecurityErrorEvent.SECURITY_ERROR, this.handleEvent);
			xstream.load(new URLRequest(url));
		}

		/**
		 * Manage the events returned by the xstream loader
		 * 
		 * @usage
		 * @param   event
		 * @return
		 */
		protected function handleEvent(event:Event):void {
			if (event.type == Event.COMPLETE) {
				var byte:ByteArray = new ByteArray();
				byte.endian = Endian.LITTLE_ENDIAN;
				event.target.readBytes(byte, 0, event.target.bytesAvailable);
				try {
					var retObject:Object = Parser.parse(byte);
					translations = retObject.translation;
					info         = retObject.info;
					charset      = retObject.charset;
				} catch (e:Error) {
					var errEvent:ErrorEvent = new ErrorEvent(ErrorEvent.ERROR, true, false, "EOFError: " + e.message);
					dispatchEvent(errEvent);
					return;
				}
			}
			var evt:Event = new Event(event.type, true, true);
			dispatchEvent(evt);
		}

		/**
		 * Try to translate a string into the current installed language
		 * 
		 * @usage
		 * @param   id the string to be translated
		 * @return  translated string, if found. Otherwise returns the passed argument
		 */
		public function _(id:String):String {
			try {
				if (translations.hasOwnProperty(id)) {
					return translations[id];
				} else {
					throw new TypeError;
				}
			} catch (e:TypeError) {
				return id;
			}
			return "";
		}
	}
}
