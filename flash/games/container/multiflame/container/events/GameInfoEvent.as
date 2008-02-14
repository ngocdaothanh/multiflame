package multiflame.container.events {
	import flash.events.Event;

	public class GameInfoEvent extends Event {
		public static const LOCAL:String = "LOCAL";

		public var id:int;
		public var channel:String;
		public var locale:String;

		// --------------------------------------------------------------------------

		public static const REMOTE:String = "REMOTE";

		public static const OK:int               = 0;
		public static const CONNECTION_ERROR:int = 1;
		public static const NO_GAME:int          = 2;

		public var code:int;
		public var info:Object;

		public function GameInfoEvent(type):void {
			super(type);
		}
	}
}