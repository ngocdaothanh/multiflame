package {
	import flash.display.Sprite;
	import flash.text.TextField;
	import flash.text.TextFormat;

	public class IntroSprite extends Sprite {
		private var _game:Game;

		public function IntroSprite(game:Game):void {
			_game = game;

			var format:TextFormat = new TextFormat();
			format.font = "_sans";
			format.size = 12;

			var text:TextField = new TextField();
			text.defaultTextFormat = format;
			text.multiline = true;
			text.x = 10;
			text.y = 10;
			text.width = 480;
			text.height = 480;
			text.wordWrap = true;
			text.htmlText = _("");
			addChild(text);
		}

		private function _(id:String):String {
			return _game.container._(id);
		}
	}
}