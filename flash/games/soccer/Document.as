package {
	import flash.display.Sprite;

	import net.web20games.game.IDocument;

	public class Document extends Sprite implements IDocument {
		private var _game:Game;

		public function Document():void {
			_game = new Game();
			addChild(_game);
		}

		public function get game():Object {
			return {klass: Game, instance: _game};
		}
	}
}