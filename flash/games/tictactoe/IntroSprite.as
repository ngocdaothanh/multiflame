package {
	import flash.display.Sprite;
	import flash.text.TextField;

	public class IntroSprite extends Sprite {
		private var _game:Game;

		public function IntroSprite(game:Game):void {
			_game = game;

			var text:TextField = new TextField();
			text.multiline = true;
			text.width = 500;
			text.height = 500;
			text.wordWrap = true;
			text.htmlText = _("Tic-tac-toe, also called noughts and crosses, hugs and kisses, and many other names, is a pencil-and-paper game for two players, O and X, who take turns to mark the spaces in a 3×3 grid. The player who succeeds in placing three respective marks in a horizontal, vertical or diagonal row wins the game.");
			addChild(text);
		}

		private function _(id:String):String {
			return _game._(id);
		}
	}
}