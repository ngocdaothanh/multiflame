package {
	import flash.display.Sprite;
	import flash.text.TextField;
	import flash.events.*;
	import flash.display.SimpleButton;
	import fl.controls.Button;

	public class Player extends Sprite {
		// The order is the same as indices in _pieces
		public static const ROCK:int     = 0;
		public static const PAPER:int    = 1;
		public static const SCISSORS:int = 2;

		// Screen center
		public static const C_X:int = 250;
		public static const C_Y:int = 250;

		// Radius
		public static const R:int = 200;

		// Just a small angle
		private static const ALPHA:Number = 15*Math.PI/180;

		private var _game:Game;
		private var _index:int;
		private var _angle:Number;
		private var _pieces:Array;
		private var _type:int;
		private var _nick:TextField;
		private var _lost:Boolean;
		
		public function Player(game:Game, index:int):void {
			_game = game;
			_index = index;
			_lost = false;

			_angle = (2*Math.PI/game.container.nicks0.length)*index;
			var me:Boolean = index == game.container.indexMe;
			
			_pieces = [_rock, _paper, _scissors];
			if (me) {
				_pieces.push(_ok);
				for (var i:int = 0; i < _pieces.length; i++) {
					_pieces[i].addEventListener(MouseEvent.CLICK, onClick);
				}
			} else {
				removeChild(_ok);
			}
			_type = 0;
			select(_type);

			_nick = new TextField();
			_nick.text = game.container.nicks0[index];
			_nick.width = 10*_nick.text.length;
			_nick.height = 18;
			_nick.x = C_X + R*Math.cos(_angle)/2;
			_nick.y = C_Y + R*Math.sin(_angle)/2;
			addChild(_nick);
		}
		
		public function select(type:int):void {
			_type = type;
			for (var i:int = 0; i < _pieces.length; i++) {
				_pieces[i].x = C_X + R*Math.cos(_angle + (i - 1)*ALPHA);
				_pieces[i].y = C_Y + R*Math.sin(_angle + (i - 1)*ALPHA);
			}
			var o:SimpleButton = _pieces[type];
			o.x = C_X + R*Math.cos(_angle)/2;
			o.y = C_Y + R*Math.sin(_angle)/2;

			_game.container.defaultMove = type;
		}

		public function get type():int {
			return _type;
		}

		public function enable():void {
			_ok.enabled = true;
		}

		public function get lost():Boolean {
			return _lost;
		}

		public function markLost():void {
			_lost = true;
			_ok.enabled = false;
			for (var i:int = 0; i < _pieces.length; i++) {
				_game.container.TweenFilterLite.to(_pieces[i], 0.5, {type: "Color", colorize: 0xCCCCCC, amount: 1});
			}
		}

		public function highlight():void {
			_game.container.TweenFilterLite.to(_pieces[_type], 0.5, {type: "Color", colorize: 0xFF0000, amount: 1});
			_game.container.TweenFilterLite.to(_pieces[_type], 0.5, {delay: 0.5, type: "Color", overwrite: false});
		}

		// ---------------------------------------------------------------------------

		private function onClick(event:MouseEvent):void {
			if (!_game.container.enabled || !_ok.enabled) {
				return;
			}

			if (event.target is Button) {
				_ok.enabled = false;
				_game.container.enqueueMove(_type);
			} else {
				select(_pieces.indexOf(event.target));
			}
		}
	}
}