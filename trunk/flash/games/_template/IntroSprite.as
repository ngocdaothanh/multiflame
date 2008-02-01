/*
*	This is template used for creating a new game.
*	TODO: Change text.htmlText to suit your need.
*/

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
			text.htmlText = _("Enter discription and rule for the game.");
			addChild(text);
		}

		private function _(id:String):String {
			return _game.translate(id);
		}
	}
}
