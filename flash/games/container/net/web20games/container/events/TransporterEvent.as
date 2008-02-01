package net.web20games.container.events {
	import flash.display.Sprite;
	import flash.events.Event;

	public class TransporterEvent extends Event {
		public static const CONNECTION_ERROR:int = -1;
		public static const CONNECTION_CLOSE:int = -2;

		public var arg:Object;

		public function TransporterEvent(cmd:int, arg:Object):void {
			super("" + cmd);
			this.arg = arg;
		}
	}
}