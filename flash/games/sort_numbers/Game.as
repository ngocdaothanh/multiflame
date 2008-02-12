package {
	import net.web20games.game.Game;
	import net.web20games.game.IContainer;

	public class Game extends net.web20games.game.Game {
		private var _boards:Array;
		private var shuffled:Boolean;

		public function Game():void {
			_boards = new Array(2);
			var numbers:Array = new Array();
			for (var i:int = 0; i < Board.WIDTH*Board.HEIGHT - 1; i++) {
				numbers.push(i);
			}
			var b1:Board = new Board(this, 0, numbers, true);
			var b2:Board = new Board(this, 0, numbers, true);
			b1.x = 33;
			b1.y = 33;
			b2.x = 266;
			b2.y = 266;
			_boards.push(b1);
			_boards.push(b2);
			addChild(b1);
			addChild(b2);
		}

		public override function get definition():Object {
			return {
				klass: CLASS_REALTIME,
				nPlayersMin: 2,
				nPlayersMax: 4,
				moveSecMin: 0,
				moveSecMax: 0,
				totalMinMin: 2,
				totalMinMax: 20
			}
		}

		public override function onContainerSet():Object {
			var introSprite:IntroSprite = new IntroSprite(this);
			return {introSprite: introSprite};
		}

		public override function onNewGame(playedBack:Boolean):int {
			if (_boards != null) {
				for (var i:int; i < _boards.length; i++) {
					if (_boards[i] != null) {
						removeChild(_boards[i]);
						_boards[i] = null;
					}
				}
			}
			_boards = new Array(baseConfig.nPlayers);

			shuffled = false;
			if (!playedBack && indexMe == 0) {
				enqueueMove(['shuffle', Board.WIDTH*Board.HEIGHT - 1]);
			}
			return A_ANY;
		}

		public override function onMove(timestamp:Number, moves:Array, playedBack:Boolean):void {
			try {
				var index:int = moves[0];
				var data:Object = moves[1];

				if (!shuffled) {
					var shuffleResult:Array = data as Array;
					for (var i:int; i < _boards.length; i++) {
						_boards[i] = new Board(this, i, data[2]);
						_boards[i].numCorrects;
						addChild(_boards[i]);
					}
					shuffled = true;
					return actionResult(A_ANY);
				}
	
				var iPiece:int = data as int;
				_boards[index].move(iPiece);
				if (_boards[index].numCorrects == Board.WIDTH*Board.HEIGHT - 1) {
					recomputeResult();
					return actionResult(A_OVER);
				}
			} catch (e:Error) {
				recomputeResult();
				return actionResult(A_OVER);
			}
			actionResult(A_ANY);
		}

		public override function onResign(timestamp:Number, index:int, playedBack:Boolean):void {
			updateGameResult(index, P_LOST);
			if (!playedBack) {
				TweenFilterLite.to(_boards[index], 0.5, {type: "Color", colorize: 0xF2DB0D, amount: 1});
			}

			// Check if there's only 1 player
			var nPlayingPlayers = nicks0.length;
			for (var i:int = 0; i < nicks0.length; i++) {
				if (gameResult[i] != P_NONE) {
					nPlayingPlayers--;
				}
			}

			if (nPlayingPlayers == 1) {
				recomputeResult();
				actionResult(A_OVER);
			} else {
				actionResult(A_ANY);
			}
		}

		public override function onTimeout(timestamp:Number, timedOut:Boolean, index:int, playedBack:Boolean):void {
			if (!timedOut) {
				updateGameResult(index, P_LOST);
				actionResult(A_ANY);
			} else {
				recomputeResult();
				actionResult(A_OVER);
			}
		}

		private function recomputeResult():String {
			var i:int;
			var index_point:Array = new Array(nicks0.length);
			for (i = 0; i < nicks0.length; i++) {
				index_point[i] = {index: i, point: _boards[i].numCorrects};
			}

			var summary:String = _("Correct numbers: ");

			// Sort, top = P_WON, bottom = P_LOST, middles = P_DREW
			index_point = index_point.sortOn("point");
			if (index_point[0].point == index_point[nicks0.length - 1].point) {
				updateGameResult(index_point[0].index, P_DREW);
				updateGameResult(index_point[nicks0.length - 1].index, P_DREW);
			} else {
				updateGameResult(index_point[0].index, P_LOST);
				updateGameResult(index_point[nicks0.length - 1].index, P_WON);
			}
			summary += nicks0[index_point[nicks0.length - 1].index] + " = " +
				index_point[nicks0.length - 1].point + " ";
			for (i = 1; i < nicks0.length - 1; i++) {
				updateGameResult(index_point[i].index, P_DREW);
				summary += nicks0[index_point[i].index] + " = " +
					index_point[i].point + " ";
			}
			summary += nicks0[index_point[0].index] + " = " + index_point[0].point;

			return summary;
		}
	}
}