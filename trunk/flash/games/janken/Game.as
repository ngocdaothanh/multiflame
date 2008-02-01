package {
	import net.web20games.game.Game;

	public class Game extends net.web20games.game.Game {
		private var _players:Array;
		private var _demoPieces:Array;

		public function Game():void {
			_demoPieces = [new Rock(), new Paper(), new Scissors()];
			var angle:Number = 120*Math.PI/180;
			for (var i:int = 0; i < 3; i++) {
				_demoPieces[i].scaleX = 2;
				_demoPieces[i].scaleY = 2;

				_demoPieces[i].x = Player.C_X + Player.R*Math.cos(0.4 + i*angle);
				_demoPieces[i].y = Player.C_Y + Player.R*Math.sin(0.4 + i*angle);

				addChild(_demoPieces[i]);
			}
		}

		public override function get definition():Object {
			return {
				klass: CLASS_BATCH,
				nPlayersMin: 2,
				nPlayersMax: 4,
				moveSecMin: 10,
				moveSecMax: 60,
				totalMinMin: 0,
				totalMinMax: 0
			};
		}

		protected override function onContainerSet():Object {
			return {};
		}

		protected override function onNewGame(playedBack:Boolean):int {
			var i:int;

			if (_demoPieces != null) {
				for (i = 0; i < _demoPieces.length; i++) {
					removeChild(_demoPieces[i]);
				}
				_demoPieces = null;
			}

			if (_players != null) {
				for (i = 0; i < _players.length; i++) {
					removeChild(_players[i]);
				}
			}

			_players = new Array(nicks0.length);
			for (i = 0; i < nicks0.length; i++) {
				_players[i] = new Player(this, i);
				addChild(_players[i]);
			}

			return A_ANY;
		}

		protected override function onMove(timestamp:Number, moves:Array, playedBack:Boolean):int {
			try {
				for (var i:int = 0; i < moves.length; i += 2) {
					var index:int = moves[i] as int;
					if (moves[i + 1] != null) {
						var type:int = moves[i + 1] as int;					
						if (type < 0 || type > 2) {
							throw new Error();
						}
						if (!_players[index].lost) {
							_players[index].select(type);
						}
					}
				}
			} catch (e:Error) {
				_players[index].markLost();
				updateGameResult(index, P_LOST);
			}

			if (checkOnePlayer() == A_ANY) {
				return checkResult();
			}
			return A_OVER;
		}

		protected override function onResign(timestamp:Number, index:int, playedBack:Boolean):int {
			_players[index].markLost();
			updateGameResult(index, P_LOST);
			return checkOnePlayer();
		}

		// ---------------------------------------------------------------------------

		private function checkResult():int {
			var i:int;
			var types:Array = new Array();

			// Check number of types
			for (i = 0; i < _players.length; i++) {
				if (!_players[i].lost && types.indexOf(_players[i].type) < 0) {
					types.push(_players[i].type);
				}
			}
			if (types.length != 2) {
				for (i = 0; i < _players.length; i++) {
					if (!_players[i].lost) {
						_players[i].enable();
					}
				}
				return A_ANY;
			}

			// Divide to a1 and a2
			var a:Array = new Array(2);
			a[0] = new Array();
			a[1] = new Array();
			for (i = 0; i < _players.length; i++) {
				if (!_players[i].lost) {
					if (_players[i].type == types[0]) {
						a[0].push(_players[i]);
					} else {
						a[1].push(_players[i]);
					}
				}
			}

			// Mark losts
			var iLosts:int;
			if (types[0] == Player.ROCK && types[1] == Player.PAPER) {
				iLosts = 0;
			} else if (types[0] == Player.ROCK && types[1] == Player.SCISSORS) {
				iLosts = 1;
			} else if (types[0] == Player.PAPER && types[1] == Player.ROCK) {
				iLosts = 1;
			} else if (types[0] == Player.PAPER && types[1] == Player.SCISSORS) {
				iLosts = 0;
			} else if (types[0] == Player.SCISSORS && types[1] == Player.ROCK) {
				iLosts = 0;
			} else if (types[0] == Player.SCISSORS && types[1] == Player.PAPER) {
				iLosts = 1;
			}
			for (i = 0; i < a[iLosts].length; i++) {
				updateGameResult(_players.indexOf(a[iLosts][i]), P_LOST);
				a[iLosts][i].markLost();
			}

			// Enable wons
			var iWons:int = 1 - iLosts;
			if (a[iWons].length == 1) {
				updateGameResult(_players.indexOf(a[iWons][0]), P_WON);
				return A_OVER;
			}
			for (i = 0; i < a[iWons].length; i++) {
				a[iWons][i].enable();
			}
			return A_ANY;
		}

		// Check if there's only one player left.
		private function checkOnePlayer():int {
			var notLosts:Array = new Array();
			for (var i = 0; i < _players.length; i++) {
				if (!_players[i].lost) {
					_players[i].highlight();
					notLosts.push(i);
				}
			}
			if (notLosts.length == 1) {
				updateGameResult(notLosts[0], P_WON);
				return A_OVER;
			}
			return A_ANY;
		}
	}
}