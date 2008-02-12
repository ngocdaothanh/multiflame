package {
	import flash.display.Sprite;

	public class IntroSprite extends Sprite {
		private var _game:Game;
		public function IntroSprite(game):void {
			_game = game;
		}

		private function _(id:String):String {
			return _game.container._(id);
		}
	}
}