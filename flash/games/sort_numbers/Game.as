package {
	import flash.display.Sprite;

	import net.web20games.game.IGame;
	import net.web20games.game.IContainer;
	import net.web20games.game.Constants;

	public class Game extends Sprite implements IGame {
		private var _boards:Array;
		private var shuffled:Boolean;

		private var _container:IContainer;

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

		public function get container():IContainer {
			return _container;
		}

		// ---------------------------------------------------------------------------

		public function get definition():Object {
			return {
				klass: Constants.REALTIME,
				nPlayersMin: 2,
				nPlayersMax: 4,
				moveSecMin: 0,
				moveSecMax: 0,
				totalMinMin: 2,
				totalMinMax: 20
			}
		}

		public function set enabled(value:Boolean):void {
		}

		public function setContainer(container:IContainer):Object {
			_container = container;
			return {introSprite: new IntroSprite(this)};
		}

		public function onNewGame(snapshot:Object):int {
			if (_boards != null) {
				for (var i:int; i < _boards.length; i++) {
					if (_boards[i] != null) {
						removeChild(_boards[i]);
						_boards[i] = null;
					}
				}
			}
			_boards = new Array(_container.baseConfig.nPlayers);

			shuffled = false;
			if (snapshot == null && _container.indexMe == 0) {
				_container.enqueueMove(['shuffle', Board.WIDTH*Board.HEIGHT - 1]);
			}
			return Constants.ANY;
		}

		public function onMove(timestamp:Number, moves:Array):void {
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
					return _container.setActionResult(Constants.ANY);
				}
	
				var iPiece:int = data as int;
				_boards[index].move(iPiece);
				if (_boards[index].numCorrects == Board.WIDTH*Board.HEIGHT - 1) {
					return _container.setActionResult(Constants.OVER, recomputeResult());
				}
			} catch (e:Error) {
				return _container.setActionResult(Constants.OVER, recomputeResult());
			}
			_container.setActionResult(Constants.ANY);
		}

		public function onResign(timestamp:Number, index:int):void {
			_container.gameResult[index] = Constants.LOST;
			_container.TweenFilterLite.to(_boards[index], 0.5, {type: "Color", colorize: 0xF2DB0D, amount: 1});

			// Check if there's only 1 player
			var nPlayingPlayers = _container.nicks0.length;
			for (var i:int = 0; i < _container.nicks0.length; i++) {
				if (_container.gameResult[i] != Constants.NONE) {
					nPlayingPlayers--;
				}
			}

			if (nPlayingPlayers == 1) {
				_container.setActionResult(Constants.OVER, recomputeResult());
			} else {
				_container.setActionResult(Constants.ANY);
			}
		}

		public function onTimeout(timestamp:Number, timedOut:Boolean, index:int):void {
			if (!timedOut) {
				_container.gameResult[index] = Constants.LOST;
				_container.setActionResult(Constants.ANY);
			} else {
				_container.setActionResult(Constants.OVER, recomputeResult());
			}
		}

		private function recomputeResult():String {
			var i:int;
			var index_point:Array = new Array(_container.nicks0.length);
			for (i = 0; i < _container.nicks0.length; i++) {
				index_point[i] = {index: i, point: _boards[i].numCorrects};
			}

			var extra:String = _container._("Correct numbers: ");

			// Sort, top = Constants.WON, bottom = Constants.LOST, middles = Constants.DREW
			index_point = index_point.sortOn("point");
			if (index_point[0].point == index_point[_container.nicks0.length - 1].point) {
				_container.gameResult[index_point[0].index] = Constants.DREW;
				_container.gameResult[index_point[_container.nicks0.length - 1].index] = Constants.DREW;
			} else {
				_container.gameResult[index_point[0].index] = Constants.LOST;
				_container.gameResult[index_point[_container.nicks0.length - 1].index] = Constants.WON;
			}
			extra += _container.nicks0[index_point[_container.nicks0.length - 1].index] + " = " +
				index_point[_container.nicks0.length - 1].point + " ";
			for (i = 1; i < _container.nicks0.length - 1; i++) {
				_container.gameResult[index_point[i].index] = Constants.DREW;
				extra += _container.nicks0[index_point[i].index] + " = " +
					index_point[i].point + " ";
			}
			extra += _container.nicks0[index_point[0].index] + " = " + index_point[0].point;

			return extra;
		}
	}
}