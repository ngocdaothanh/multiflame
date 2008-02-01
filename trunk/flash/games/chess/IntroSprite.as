/*
*	This is template used for creating a new game.
*	TODO: Change text.htmlText to suit your need.
*/

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
			text.htmlText = _("Chess is a recreational and competitive game played between two players. Sometimes called Western Chess or International Chess to distinguish it from its predecessors and other chess variants, the current form of the game emerged in Southern Europe during the second half of the 15th century after evolving from similar, much older games of Indian and Persian origin. Today, chess is one of the world's most popular games, played by millions of people worldwide in clubs, online, by correspondence, in tournaments and informally. Readmore: http://en.wikipedia.org/wiki/Chess");
			addChild(text);
		}

		private function _(id:String):String {
			return _container.translate(id);
		}
	}
}
