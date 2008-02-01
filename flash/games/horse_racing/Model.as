/*
*	This is template used for creating a new game.
*	TODO: Follow intructions (numbered-comment).
*/

package {
	import net.web20games.game.Model;

	public class Model extends net.web20games.game.Model {
		// [1] Needed variables come here.

		public function Model(baseConfig:Object, extendedConfig:Object):void {
			super(baseConfig, extendedConfig);

			// [2] Initialize needed variables.
		}
		
		// --------------------------------------------------------------------------

		// DO NOT modify this function.
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

		public override function onMove(index:int, data:Object):int {
			var ret:int;
			try {
				// [3] Enter validation code here.	

				// [4] Enter code to make the move here.
				
				// [5] implement computeResult function.
				// ret = computeResult(your_data_to_pass_to_function);
			} catch (e:Error) { // DO NOT modify this part.				
				updateGameResult(index, LOST);
				updateGameResult(1 - index, WON);
				ret = GAME_OVER;
			}
			return ret;
		}

		// DO NOT modify this function.
		public override function onLeave(index:int):int {
			updateGameResult(index, LOST);
			updateGameResult(1 - index, WON);
			return GAME_OVER;
		}

		// DO NOT modify this function
		public override function onTimeout():int {
			updateGameResult(_actionResult, LOST);
			updateGameResult(1 - _actionResult, WON);
			return GAME_OVER;
		}

		// [5] Implement this function to determine the winner of the game.
		private function computeResult(index:int):int {			
			// If (Fist_player_won) {
			//	updateGameResult(0, WON);
			//	updateGameResult(1, LOST);
			//	return GAME_OVER;
			//}
			
			// If (Second_player_won) {			
			//	updateGameResult(0, LOST);
			//	updateGameResult(1, WON);
			//	return GAME_OVER;
			//}			

			// If (Drew) {			
			//	updateGameResult(0, DREW);
			//	updateGameResult(1, DREW);
			//	return GAME_OVER;
			//}

			// Next player
			return (1 - index);
		}
	}
}
