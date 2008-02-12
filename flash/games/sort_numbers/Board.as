﻿package {
	import flash.display.Sprite;
	import flash.text.TextField;
	import flash.text.TextFormat;
	import flash.events.MouseEvent;

	public class Board extends Sprite {
		public static const WIDTH:int  = 5;
		public static const HEIGHT:int = 5;

		private var _game:Game;
		private var _index:int;
		private var _nick:TextField;
		private var _pieces:Array;

		public function Board(game:Game, index:int, numbers:Array, demo:Boolean = false):void {
			_game = game;
			_index = index;

			var format:TextFormat = new TextFormat();
			format.font = "_sans";
			format.size = 12;

			if (!demo) {
				_nick = new TextField();
				_nick.defaultTextFormat = format;
				_nick.width = 200;
				_nick.height = 20;
				_nick.y = -20;			
				_nick.text = game.nicks0[index];
				addChild(_nick);
			}
			
			_pieces = new Array(WIDTH*HEIGHT);
			for (var i:int = 0; i < WIDTH*HEIGHT - 1; i++) {
				_pieces[i] = new Piece();
				_pieces[i].number = numbers[i];
				_pieces[i].x = Piece.SIZE*int(i%WIDTH);
				_pieces[i].y = Piece.SIZE*int(i/WIDTH);
				_pieces[i].addEventListener(MouseEvent.CLICK, movePiece);
				addChild(_pieces[i]);
			}
			_pieces[i] = null;

			if (!demo) {
				var nPlayers:int = game.baseConfig.nPlayers;
				if (index == 0) {
					x = 33;
					y = 33;
				} else if (index == 1) {
					x = 266;
					y = (nPlayers == 2)? 266 : 33;
				} else if (index == 2) {
					x = 266;
					y = 266;
				} else {
					x = 33;
					y = 266;
				}
			}
		}

		public function move(iPiece):void {
			if (isTop(iPiece)) {
				moveDown(iPiece);
			} else if (isRight(iPiece)) {
				moveLeft(iPiece);
			} else if (isBottom(iPiece)) {
				moveUp(iPiece);
			} else if (isLeft(iPiece)) {
				moveRight(iPiece);
			}
		}

		public function get numCorrects():int {
			var ret:int = 0;
			for (var i:int = 0; i < WIDTH*HEIGHT; i++) {
				if (_pieces[i] != null) {
					if (_pieces[i].number == i) {
						_game.TweenFilterLite.to(_pieces[i], 0.5, {type: "Color", colorize: 0x00FF00, amount: 1});
						ret++;
					} else {
						_game.TweenFilterLite.to(_pieces[i], 0.5, {type: "Color"});
					}
				}
			}
			return ret;
		}

		// ---------------------------------------------------------------------------

		private function movePiece(event:MouseEvent):void {
			if (!_game.enabled || _game.indexMe != _index) {
				return;
			}

			// Which piece?
			var iPiece:int;
			for (iPiece = 0; iPiece < WIDTH*HEIGHT; iPiece++) {
				if (event.target == _pieces[iPiece] ||
						(_pieces[iPiece] != null && _pieces[iPiece].contains(event.target))) {
					break;
				}
			}
			if (iPiece == WIDTH*HEIGHT) {
				return;
			}

			move(iPiece);
			_game.enqueueMove(iPiece);
		}

		// ---------------------------------------------------------------------------

		private function isTop(iPiece:int):Boolean {
			return (iPiece + WIDTH < WIDTH*HEIGHT && _pieces[iPiece + WIDTH] == null);
		}

		private function isRight(iPiece:int):Boolean {
			return (iPiece - 1 >= 0 && _pieces[iPiece - 1] == null && iPiece%WIDTH != 0);
		}

		private function isBottom(iPiece:int):Boolean {
			return (iPiece - WIDTH >= 0 && _pieces[iPiece - WIDTH] == null);
		}

		private function isLeft(iPiece:int):Boolean {
			return (iPiece + 1 < WIDTH*HEIGHT && _pieces[iPiece + 1] == null && (iPiece + 1)%WIDTH != 0);
		}

		// ---------------------------------------------------------------------------

		private function moveDown(iPiece:int):void {
			_pieces[iPiece].y += Piece.SIZE;
			_pieces[iPiece + WIDTH] = _pieces[iPiece];
			_pieces[iPiece] = null;
		}
		
		private function moveLeft(iPiece:int):void {
			_pieces[iPiece].x -= Piece.SIZE;
			_pieces[iPiece - 1] = _pieces[iPiece];
			_pieces[iPiece] = null;
		}
		
		private function moveUp(iPiece:int):void {
			_pieces[iPiece].y -= Piece.SIZE;
			_pieces[iPiece - WIDTH] = _pieces[iPiece];
			_pieces[iPiece] = null;
		}
		
		private function moveRight(iPiece:int):void {
			_pieces[iPiece].x += Piece.SIZE;
			_pieces[iPiece + 1] = _pieces[iPiece];
			_pieces[iPiece] = null;
		}
	}
}