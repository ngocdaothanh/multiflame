package {
	import flash.display.Sprite;
	import flash.events.MouseEvent;
	import flash.text.TextField;
	import flash.text.TextFormat;

	import net.web20games.game.Game;
	import net.web20games.game.IContainer;

	public class Game extends net.web20games.game.Game {
		private var _nick0:TextField;
		private var _nick1:TextField;
		private var _pieces:Array;
		private var _hint:Array;

		public function Game():void {
			enabled = false;

			var format:TextFormat = new TextFormat();
			format.font = "_sans";
			format.size = 12;

			_nick0 = new TextField();
			_nick1 = new TextField();
			_nick0.defaultTextFormat = format;
			_nick1.defaultTextFormat = format;
			_nick0.selectable = false;
			_nick1.selectable = false;
			_nick0.text = "";
			_nick1.text = "";

			var p:Piece;

			_nick0.x = Piece.S + Piece.PADDING + 10;
			_nick0.y = Piece.S/2 - 10;
			addChild(_nick0);
			p = new Piece(Piece.BLACK, false);
			p.x = 10;
			p.y = 0;
			addChild(p);

			_nick1.x = _nick0.x;
			_nick1.y = 500 - _nick0.y - 18;
			addChild(_nick1);
			p = new Piece(Piece.WHITE, false);
			p.x = 10;
			p.y = 500 - Piece.S;
			addChild(p);

			_pieces = new Array(8*8);
			var padding:int = (500 - 8*Piece.S)/2;
			for (var i:int = 0; i < 8*8; i++) {
				p = new Piece(Piece.EMPTY);
				p.x = padding + Piece.S*(i%8);
				p.y = padding + Piece.S*int(i/8);
				p.addEventListener(MouseEvent.CLICK, onClick);
				addChild(p);
				_pieces[i] = p;
			}

			// Put the 4 pieces
			_pieces[27].state = Piece.WHITE;
			_pieces[28].state = Piece.BLACK;
			_pieces[35].state = Piece.BLACK;
			_pieces[36].state = Piece.WHITE;
		}

		public override function get definition():Object {
			return {
				klass: CLASS_TURN_BASED,
				nPlayersMin: 2,
				nPlayersMax: 2,
				moveSecMin: 10,
				moveSecMax: 60,
				totalMinMin: 3,
				totalMinMax: 20
			};
		}

		protected override function onContainerSet():Object {
			return {introSprite: new IntroSprite(this)};
		}

		protected override function onNewGame(playedBack:Boolean):int {
			var i:int;

			// Display nicks
			_nick0.htmlText = nicks0[0];
			_nick1.htmlText = nicks0[1];

			// Clear
			for (i = 0; i < 8*8; i++) {
				_pieces[i].state = Piece.EMPTY;
			}

			// Put the 4 pieces
			_pieces[27].state = Piece.WHITE;
			_pieces[28].state = Piece.BLACK;
			_pieces[35].state = Piece.BLACK;
			_pieces[36].state = Piece.WHITE;

			// Hints
			_hint = hint(Piece.BLACK);
			for (i = 0; i < _hint.length; i++) {
				_pieces[_hint[i]].state = Piece.HINT;
			}

			return 0;
		}

		protected override function onMove(timestamp:Number, moves:Array, playedBack:Boolean):int {
			var index:int;
			var iPiece:int;
			var ret:int;
			var i:int;

			try {
				index = moves[0] as int;
				iPiece = moves[1] as int;

				// Validation
				if (_hint.indexOf(iPiece) < 0) {
					throw new Error();
				}

				// Clear hints
				for (i = 0; i < _hint.length; i++) {
					_pieces[_hint[i]].state = Piece.EMPTY;
				}

				// Make the move
				doMove(index, iPiece);

				// Hint and check result
				ret = 1 - lastActionResult;
				_hint = hint((ret == 0)? Piece.BLACK : Piece.WHITE);
				if (_hint.length > 0) {
					for (i = 0; i < _hint.length; i++) {
						_pieces[_hint[i]].state = Piece.HINT;
					}
				} else {
					ret = 1 - ret;
					_hint = hint((ret == 0)? Piece.BLACK : Piece.WHITE);
					if (_hint.length > 0) {
						for (i = 0; i < _hint.length; i++) {
							_pieces[_hint[i]].state = Piece.HINT;
						}
					} else {
						computeResult();
						ret = A_OVER;
					}
				}
			} catch (e:Error) {
				updateGameResult(index, P_LOST);
				updateGameResult(1 - index, P_WON);
				ret = A_OVER;
			}

			return ret;
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

		// --------------------------------------------------------------------------
		
		private function onClick(event:MouseEvent):void {
			if (!enabled) {
				return;
			}

			// Whick piece was clicked?
			var iPiece:int;
			for (iPiece = 0; iPiece < 8*8; iPiece++) {
				if (event.target == _pieces[iPiece]) {
					break;
				}
			}
			if (iPiece == 8*8) {
				return;
			}

			// The only positions where the piece can be put has been hinted
			if (_pieces[iPiece].state != Piece.HINT) {
				return;
			}

			_pieces[iPiece].state = (indexMe == 0)? Piece.BLACK : Piece.WHITE;
			move(iPiece);
		}

		// --------------------------------------------------------------------------

		/**
		 * @param side Piece.BLACK or Piece.WHITE
		 * @returns Array of index of pieces that a side can put.
		 */
		private function hint(side:int):Array {
			var ret:Array = new Array();
			for (var i:int = 0; i < 8*8; i++) {
				if (_pieces[i].state != Piece.EMPTY) {
					continue;
				}
				for (var j:int = 0; j < 8*8; j++) {
					if (_pieces[j].state != side) {
						continue;
					}
					if ((checkH(side, j, i)).length > 0) {
						ret.push(i);
						break;
					} else if (checkV(side, j, i).length > 0) {
						ret.push(i);
						break;
					} else if (checkX(side, j, i).length > 0) {
						ret.push(i);
						break;
					}
				}
			}
			return ret;
		}

		/**
		 * @return Array of indices of opponents.
		 */
		private function checkH(side:int, iPiece:int, iPiece2:int):Array {
			// Same row?
			if (int(iPiece/8) != int(iPiece2/8)) {
				return new Array();
			}

			// Far enough?
			if (Math.abs(iPiece - iPiece2) < 2) {
				return new Array();
			}

			// Check from iPiece -> iPiece2
			var step:int = (iPiece < iPiece2)? 1 : -1;
			iPiece += step;
			var opponent:int = Piece.BLACK + Piece.WHITE - side;
			var ret = new Array();
			while (iPiece != iPiece2) {
				if (_pieces[iPiece].state != opponent) {
					return new Array();
				}
				ret.push(iPiece);
				iPiece += step;
			}
			return ret;
		}

		private function checkV(side:int, iPiece:int, iPiece2:int):Array {
			// Same column?
			if (iPiece%8 != iPiece2%8) {
				return new Array();
			}

			// Far enough?
			if (Math.abs(iPiece - iPiece2) < 2*8) {
				return new Array();
			}

			// Check from iPiece -> iPiece2
			var step:int = (iPiece < iPiece2)? 8 : -8;
			iPiece += step;
			var opponent:int = Piece.BLACK + Piece.WHITE - side;
			var ret = new Array();
			while (iPiece != iPiece2) {
				if (_pieces[iPiece].state != opponent) {
					return new Array();
				}
				ret.push(iPiece);
				iPiece += step;
			}
			return ret;
		}

		private function checkX(side:int, iPiece:int, iPiece2:int):Array {
			var r:int = int(iPiece/8);
			var c:int = iPiece%8;
			var r2:int = int(iPiece2/8);
			var c2:int = iPiece2%8;

			// Same X?
			if (Math.abs(r - r2) != Math.abs(c - c2)) {
				return new Array();
			}

			// Far enough?
			if (r - r2 == c - c2) {
				if (Math.abs(iPiece - iPiece2) < 2*9) {
					return new Array();
				}
			} else {
				if (Math.abs(iPiece - iPiece2) < 2*7) {
					return new Array();
				}
			}

			// Check from iPiece -> iPiece2
			var step:int = (r - r2 == c - c2)? 9 : 7;
			if (iPiece > iPiece2) {
				step = -step;
			}
			iPiece += step;
			var opponent:int = Piece.BLACK + Piece.WHITE - side;
			var ret:Array = new Array();
			while (iPiece != iPiece2) {
				if (_pieces[iPiece].state != opponent) {
					return new Array();
				}
				ret.push(iPiece);
				iPiece += step;
			}
			return ret;
		}

		private function doMove(index, iPiece:int):void {
			var ret:int;
			var i:int;

			var side:int = (index == 0)? Piece.BLACK : Piece.WHITE;
			var iOpponents:Array = new Array();
			for (i = 0; i < 8*8; i++) {
				if (_pieces[i].state == side) {
					iOpponents = iOpponents.concat(checkH(side, i, iPiece));
					iOpponents = iOpponents.concat(checkV(side, i, iPiece));
					iOpponents = iOpponents.concat(checkX(side, i, iPiece));
				}
			}
			_pieces[iPiece].state = side;
			for (i = 0; i < iOpponents.length; i++) {
				_pieces[iOpponents[i]].state = side;
			}
		}

		private function computeResult():void {
			var blacks:int = 0;
			var whites:int = 0;
			for (var i:int = 0; i < 8*8; i++) {
				if (_pieces[i].state == Piece.BLACK) {
					blacks++;
				} else if (_pieces[i].state == Piece.WHITE) {
					whites++;
				}
			}
			if (blacks > whites) {
				updateGameResult(0, P_WON);
				updateGameResult(1, P_LOST);
			} else if (blacks < whites) {
				updateGameResult(0, P_LOST);
				updateGameResult(1, P_WON);
			} else {
				updateGameResult(0, P_DREW);
				updateGameResult(1, P_DREW);
			}
		}
	}
}