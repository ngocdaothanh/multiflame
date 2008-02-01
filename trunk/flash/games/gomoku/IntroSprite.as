package {
	import flash.display.Sprite;
	import flash.text.TextField;

	import net.web20games.game.IContainer;

	public class IntroSprite extends Sprite {
		private var _container:IContainer;

		public function IntroSprite(container:IContainer):void {
			_container = container;

			var text:TextField = new TextField();
			text.multiline = true;
			text.width = 500;
			text.height = 500;
			text.wordWrap = true;
			text.htmlText = _("The winner is the first player to get an unbroken row of five pieces horizontally, vertically, or diagonally.");
			addChild(text);
		}

		private function _(id:String):String {
			return _container.translate(id);
		}
	}
}
