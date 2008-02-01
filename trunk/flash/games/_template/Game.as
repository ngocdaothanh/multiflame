/*
*	This is template used for creating a new game.
*	TODO: Follow intructions (numbered-comment).
*/

package {
	import flash.display.Sprite;
	import flash.events.MouseEvent;
	import flash.text.TextField;

	import net.web20games.game.Game;
	import net.web20games.game.IContainer;

	public class Game extends net.web20games.game.Game {		
		// [1] Needed variables come here.

		public function Game():void {
			enabled = false;
			_nick0.selectable = false;
			_nick1.selectable = false;
			_nick0.htmlText = "";
			_nick1.htmlText = "";
			_board.addEventListener(MouseEvent.CLICK, onClick);
		}

		// Do not edit this function.
		public override function get baseConfigRanges():Object {
			return {
				nPlayersMin: 2,
				nPlayersMax: 2,
				oneMoveTimeMin: 10,
				oneMoveTimeMax: 60,
				totalTimeMin: 0,
				totalTimeMax: 0,
				batchMode: false};
		}

		// Do not edit this function.
		protected override function onContainerSet():Object {
			return {introSprite: new IntroSprite(this)};
		}

		protected override function onNewGame():int {
			// [2] Initialize variables.

			// Display nicks
			_nick0.htmlText = nicks0[0];
			_nick1.htmlText = nicks0[1];

			return 0;
		}

		protected override function onMove(timestamp:int, moves:Array, playedBack:Boolean):int {
			var index:int;			
			var ret:int;

			try {
				index = moves[0] as int;
				//gameData = moves[1] as int;

				// [3] Validation code comes here.
				

				// [4] Make the move.

				// [5] Compute result.
				//ret = computeResult(index);
				enabled = ret >= 0 && ret == indexMe;
			} catch (e:Error) { // Dont edit this part.
				updateGameResult(index, LOST);
				updateGameResult(1 - index, WON);
				ret = GAME_OVER;
			}
			return ret;
		}

		// Do not edit this function.
		protected override function onResign(timestamp:int, index:int, playedBack:Boolean):int {
			updateGameResult(index, LOST);
			updateGameResult(1 - index, WON);
			return GAME_OVER;
		}

		// Do not edit this function.
		protected override function onTimeout(timestamp:int, index:int, playedBack:Boolean):int {
			updateGameResult(lastActionResult, LOST);
			updateGameResult(1 - lastActionResult, WON);
			return GAME_OVER;
		}

		// --------------------------------------------------------------------------

		// [6] Implement this function.
		private function computeResult(index:int):int {
			return (1-index);
		}

		// --------------------------------------------------------------------------
		
		// [7] Implement this function.
		private function onClick(event:MouseEvent):void {
			if (!enabled) {
				return;
			}

			// [8] Other code come here.
		}		
	}
}