package {
	import flash.display.Sprite;
	import flash.events.MouseEvent;
	import flash.text.TextField;

	import net.web20games.game.IGame;
	import net.web20games.game.IContainer;
	import net.web20games.game.Constants;

	public class Game extends Sprite implements net.web20games.game.IGame {
		private var _pieces:Array;
		private var _container:IContainer;

		public function Game():void {
			_nick0.selectable = false;
			_nick1.selectable = false;
			_nick0.htmlText = "";
			_nick1.htmlText = "";
			_board.addEventListener(MouseEvent.CLICK, onClick);
		}

		public function get container():IContainer {
			return _container;
		}

		// ---------------------------------------------------------------------------

		public function get definition():Object {
			return {
				klass: Constants.TURN_BASED,
				nPlayersMin: 2,
				nPlayersMax: 2,
				moveSecMin: 10,
				moveSecMax: 60,
				totalMinMin: 0,
				totalMinMax: 0
			};
		}

		public function setContainer(container:IContainer):Object {
			_container = container;
			return {introSprite: new IntroSprite(this)};
		}

		public function onNewGame(snapshot:Object):int {
			var i:int;

			if (_pieces != null) {
				for (i = 0; i < _pieces.length; i++) {
					if (_pieces[i] != null) {
						removeChild(_pieces[i]);
					}
				}
			}
			_pieces = new Array(9);

			// Display nicks
			_nick0.htmlText = _container.nicks0[0];
			_nick1.htmlText = _container.nicks0[1];

			return 0;
		}

		public function onMove(timestamp:Number, moves:Array):void {
			var index:int;
			var iPiece:int;
			var ret:int;

			try {
				index = moves[0] as int;
				iPiece = moves[1] as int;

				// Validation
				if (iPiece < 0 || iPiece > 8 || _pieces[iPiece] != null) {
					throw new Error();
				}

				// Make the move
				addPiece((index == 0)? new O() : new X(), iPiece, false);

				ret = computeResult(index);
			} catch (e:Error) {
				_container.gameResult[index] = Constants.LOST;
				_container.gameResult[1 - index] = Constants.WON;
				ret = Constants.OVER;
			}
			_container.setActionResult(ret);
		}

		public function onResign(timestamp:Number, index:int):void {
			_container.gameResult[index] = Constants.LOST;
			_container.gameResult[1 - index] = Constants.WON;
			_container.setActionResult(Constants.OVER);
		}

		public function onTimeout(timestamp:Number, timedOut:Boolean, index:int):void {
			_container.gameResult[index] = Constants.LOST;
			_container.gameResult[1 - index] = Constants.WON;
			_container.setActionResult(Constants.OVER);
		}

		// --------------------------------------------------------------------------

		private function onClick(event:MouseEvent):void {
			if (_container == null || !_container.enabled) {
				return;
			}

			var i:int = xy2i(event.localX, event.localY);
			if (i >= 0) {
				if (_pieces[i] == null) {
					_container.enqueueMove(i);
				}
			}
		}

		// --------------------------------------------------------------------------

		private function computeResult(index:int):int {
			// Horizontal
			if ((_pieces[0] is O && _pieces[1] is O && _pieces[2] is O) ||
				(_pieces[3] is O && _pieces[4] is O && _pieces[5] is O) ||
				(_pieces[6] is O && _pieces[7] is O && _pieces[8] is O)) {
				_container.gameResult[0] = Constants.WON;
				_container.gameResult[1] = Constants.LOST;
				return Constants.OVER;
			}
			if ((_pieces[0] is X && _pieces[1] is X && _pieces[2] is X) ||
				(_pieces[3] is X && _pieces[4] is X && _pieces[5] is X) ||
				(_pieces[6] is X && _pieces[7] is X && _pieces[8] is X)) {
				_container.gameResult[0] = Constants.LOST;
				_container.gameResult[1] = Constants.WON;
				return Constants.OVER;
			}

			// Vertical
			if ((_pieces[0] is O && _pieces[3] is O && _pieces[6] is O) ||
				(_pieces[1] is O && _pieces[4] is O && _pieces[7] is O) ||
				(_pieces[2] is O && _pieces[5] is O && _pieces[8] is O)) {
				_container.gameResult[0] = Constants.WON;
				_container.gameResult[1] = Constants.LOST;
				return Constants.OVER;
			}
			if ((_pieces[0] is X && _pieces[3] is X && _pieces[6] is X) ||
				(_pieces[1] is X && _pieces[4] is X && _pieces[7] is X) ||
				(_pieces[2] is X && _pieces[5] is X && _pieces[8] is X)) {
				_container.gameResult[0] = Constants.LOST;
				_container.gameResult[1] = Constants.WON;
				return Constants.OVER;
			}

			// Crossed
			if ((_pieces[0] is O && _pieces[4] is O && _pieces[8] is O) ||
				(_pieces[2] is O && _pieces[4] is O && _pieces[6] is O)) {
				_container.gameResult[0] = Constants.WON;
				_container.gameResult[1] = Constants.LOST;
				return Constants.OVER;
			}
			if ((_pieces[0] is X && _pieces[4] is X && _pieces[8] is X) ||
				(_pieces[2] is X && _pieces[4] is X && _pieces[6] is X)) {
				_container.gameResult[0] = Constants.LOST;
				_container.gameResult[1] = Constants.WON;
				return Constants.OVER;
			}

			// Drew
			var count:int = 0;
			for (var i:int = 0; i < 9; i++) {
				if (_pieces[i] != null) {
					count++;
				}
			}
			if (count == 9) {
				_container.gameResult[0] = Constants.DREW;
				_container.gameResult[1] = Constants.DREW;
				return Constants.OVER;
			}

			return (1 - index);
		}

		private function xy2i(x:int, y:int):int {
			if (x < 0 || y < 0 || x >= 300 || y >= 300) {
				return -1;
			}
			var r:int = int(y/100);
			var c:int = int(x/100);
			return (r*3 + c);
		}

		private function addPiece(piece:Sprite, i:int, playedBack:Boolean) {
			var r:int = i/3;
			var c:int = i%3;
			piece.y = r*100 + 50 + _board.x;
			piece.x = c*100 + 50 + _board.y;
			addChild(piece);
			_pieces[i] = piece;

			if (!playedBack) {
				_container.TweenLite.to(piece, 0.5, {scaleX: 1.5, scaleY: 1.5});
				_container.TweenLite.to(piece, 0.5, {delay: 0.5, scaleX: 0.5, scaleY: 0.5, overwrite: false});
				_container.TweenLite.to(piece, 0.5, {delay: 1, scaleX: 1, scaleY: 1, overwrite: false});
			}
		}
	}
}