package {
	import flash.display.Sprite;
	import flash.events.MouseEvent;
	import flash.text.TextField;

	import net.web20games.game.Game;
	import net.web20games.game.IContainer;

	public class Game extends net.web20games.game.Game {
		public function Game():void {
			enabled = false;
			_nick0.selectable = false;
			_nick1.selectable = false;
			_nick0.htmlText = "";
			_nick1.htmlText = "";
		}

		public override function get definition():Object {
			return {
				klass: CLASS_BATCH,
				nPlayersMin: 2,
				nPlayersMax: 2,
				moveSecMin: 10,
				moveSecMax: 60,
				totalMinMin: 0,
				totalMinMax: 0
			};
		}

		protected override function onContainerSet():Object {
			return {introSprite: new IntroSprite(this)};
		}

		protected override function onNewGame(playedBack:Boolean):int {
			// Display nicks
			_nick0.htmlText = nicks0[0];
			_nick1.htmlText = nicks0[1];

			return 0;
		}

		protected override function onMove(timestamp:Number, moves:Array, playedBack:Boolean):int {
			return A_ANY;
		}

		protected override function onResign(timestamp:Number, index:int, playedBack:Boolean):int {
			updateGameResult(index, P_LOST);
			updateGameResult(1 - index, P_WON);
			return A_OVER;
		}

		protected override function onTimeout(timestamp:Number, timedOut:Boolean, index:int, playedBack:Boolean):int {
			updateGameResult(index, P_LOST);
			updateGameResult(1 - index, P_WON);
			return A_OVER;
		}
	}
}