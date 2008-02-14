package multiflame.container.events {
	import flash.events.Event;

	public class GameOverEvent extends Event {
		public static const GAME_OVER:String = "GAME_OVER";

		public function GameOverEvent():void {
			super(GAME_OVER);
		}
	}
}