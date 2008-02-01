package {
	import flash.display.Sprite;

	public class Piece extends Sprite {
		// States
		public static const EMPTY:int = 0;
		public static const HINT:int =  1;
		public static const BLACK:int = 2;
		public static const WHITE:int = 3;

		public static const S:int       = 50;
		public static const PADDING:int = 5;
		
		private var _state:int;
		private var _withBorder:Boolean;

		public function Piece(state:int, withBorder:Boolean = true):void {
			_withBorder = withBorder;
			this.state = state;
		}

		public function set state(value:int):void {
			_state = value;

			graphics.clear();
			graphics.lineStyle(1, 0x000000);

			// Paint empty
			if (_withBorder) {
				graphics.beginFill(0xFFFFFF);
	            graphics.drawRect(0, 0, S, S);
			}

			switch (_state) {
			case EMPTY:
				// Drawn above
				break;
			case HINT:
				graphics.beginFill(0xCCCCCC);
	            graphics.drawRect(0, 0, S, S);
				break;
			case BLACK:
				graphics.beginFill(0x000000);
	            graphics.drawCircle(S/2, S/2, S/2 - PADDING);
				break;
			case WHITE:
	            graphics.drawCircle(S/2, S/2, S/2 - PADDING);
				break;
			}
			graphics.endFill();
		}

		public function get state():int {
			return _state;
		}
	}
}