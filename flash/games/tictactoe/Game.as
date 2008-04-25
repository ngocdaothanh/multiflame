﻿package {
	import flash.display.Sprite;
	import flash.events.MouseEvent;
	import flash.text.TextField;

	import multiflame.game.IGame;
	import multiflame.game.IContainer;
	import multiflame.game.Constants;

	public class Game extends Sprite implements IGame {
		private var _pieces:Array;
		private var _container:IContainer;
		private var _enabled:Boolean;

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

		public function get baseConfigRange():Object {
			return {
				nPlayersMin: 2, nPlayersMax: 2,
				moveSecMin: 10, moveSecMax: 60,
				totalMinMin: 0,	totalMinMax: 0
			};
		}

		public function setContainer(container:IContainer):Object {
			_container = container;
			return {
				type: Constants.IGOUGO,
				introSprite: new IntroSprite(this)
			};
		}

		public function set enabled(value:Boolean):void {
			_enabled = value;
		}

		public function get snapshot():Object {
			var ret:Array = new Array(9);
			for (var i = 0; i < 9; i++) {
				if (_pieces[i] is O) {
					ret[i] = 0;
				} else if (_pieces[i] is X) {
					ret[i] = 1;
				} else {
					ret[i] = null;
				} 
			}
			return ret;
		}

		public function onNewGame(snapshot:Object):void {
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

			restore(snapshot);
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
				addPiece(index, iPiece, false);

				ret = computeResult(index);
			} catch (e:Error) {
				_container.gameResult[index] = Constants.LOST;
				_container.gameResult[1 - index] = Constants.WON;
				ret = Constants.OVER;
			}
			_container.onActionDone(ret);
		}

		public function onResign(timestamp:Number, index:int):void {
			_container.gameResult[index] = Constants.LOST;
			_container.gameResult[1 - index] = Constants.WON;
			_container.onActionDone(Constants.OVER);
		}

		public function onTimeout(timestamp:Number, timedOut:Boolean, index:int):void {
			_container.gameResult[index] = Constants.LOST;
			_container.gameResult[1 - index] = Constants.WON;
			_container.onActionDone(Constants.OVER);
		}

		// --------------------------------------------------------------------------

		private function onClick(event:MouseEvent):void {
			if (_container == null || !_enabled) {
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

		private function restore(snapshot:Object):void {
			var count0:int = 0;
			var count1:int = 0;
			if (snapshot != null) {
				for (var i:int = 0; i < 9; i++) {
					if (snapshot[i] == 0) {
						count0++;
						addPiece(0, i, true);
					} else if (snapshot[i] == 1) {
						count1++;
						addPiece(1, i, true);
					}
				}
			}

			_container.onActionDone((count0 == count1)? 0 : 1);
		}

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

		private function addPiece(index:int, i:int, playedBack:Boolean) {
			var piece:Sprite = (index == 0)? new O() : new X();
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