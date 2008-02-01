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
			text.htmlText = _("Chinese chess or xiangqi is one of the most popular board games in the world. Distinctive features of Xiangqi include the unique movement of the pao (cannon) piece, a rule prohibiting the generals (similar to chess kings) from facing each other directly, and the river and palace board features, which restrict the movement of some pieces. For more information, visit: http://en.wikipedia.org/wiki/Xiangqi");
			addChild(text);
		}

		private function _(id:String):String {
			return _container.translate(id);
		}
	}
}
