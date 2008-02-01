package net.web20games.container.events {
	import flash.events.Event;

	public class ResultEvent extends Event {
		public static const RESULT:String = "GAME_RESULT";

		public var result:Array;

		public function ResultEvent(result:Array):void {
			super(RESULT);
			this.result = result;
		}
	}
}
