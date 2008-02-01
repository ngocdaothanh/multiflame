package {
	import flash.display.Sprite;
	import flash.text.TextField;

	public class Piece extends Sprite {
		public static const SIZE:int = 40;

		public function set number(value:int):void {
			_number.selectable = false;
			_number.text = "" + value;
		}

		public function get number():int {
			return int(_number.text);
		}
	}
}