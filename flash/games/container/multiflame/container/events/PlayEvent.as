package multiflame.container.events {
	import flash.events.Event;

	public class PlayEvent extends Event {
		// Seconds from the game start.
		public var timestamp:Number;

		public static const MOVE:String    = "PLAY_MOVE";
		public var moves:Array;  // [index, data, index, data...]

		public static const RESIGN:String  = "PLAY_RESIGN";
		public static const TIMEOUT:String = "PLAY_TIMEOUT";
		public var index:int;  // Index of player

		// Dispatched by the game container to notify about the result of the last
		// action. This event is used by the timeout status bar.
		public static const ACTION_RESULT:String = "PLAY_ACTION_RESULT";
		public var actionResult:int;
		public var moveSecLeft:int;
		public var totalSecLeft:int;

		public function PlayEvent(type:String):void {
			super(type);
		}
	}
}