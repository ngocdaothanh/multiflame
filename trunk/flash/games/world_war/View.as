package {
	import flash.display.Sprite;
	
	public class View extends Sprite {
		public static const S:int = 30;                // Size of one square, [pixel]
		private static const COLOR_B:uint = 0x0099FF;  // Background
		private static const COLOR_O:uint = 0xFF0000;
		private static const COLOR_X:uint = 0x0000FF;
		private static const COLOR_H:uint = 0x00FF00;  // Highlight
		private static const COLOR_F:uint = 0x0099AA;  // Flight
		private static const COLOR_E:uint = 0x333333;  // Explode

		public function drawBoard():void {
			graphics.clear();
			graphics.beginFill(COLOR_B, 0.5);
            graphics.drawRect(0, 0, S*Model.W, S*Model.H);
			graphics.endFill();
			
			var i:int;
			graphics.lineStyle(1, 0x000000);
			// Horizontal lines
			for (i = 0; i < Model.H + 1; i++) {
				graphics.moveTo(0, i*S);
				graphics.lineTo(S*Model.W, i*S);
			}
			// Vertical lines
			for (i = 0; i < Model.W + 1; i++) {
				graphics.moveTo(i*S, 0);
				graphics.lineTo(i*S, S*Model.H);
			}            
		}
		
		public function drawPiece(p:int, r:int, c:int):void {
			if (p == Model.P_O) {
				drawO(this, S*c, S*r);
			} else {
				drawX(this, S*c, S*r);
			}
		}
		
		public function drawHighLightPiece(p:int, r:int, c:int):void {
			if (p == Model.P_O) {
				drawO(this, S*c, S*r, COLOR_H);
			} else {
				drawX(this, S*c, S*r, COLOR_H);
			}
		}
		
		public function drawFlight(r:int, c:int):void {
			drawO(this, S*c, S*r, COLOR_F);
		}
		
		public function drawExplode(r:int, c:int):void {
			drawO(this, S*c, S*r, COLOR_E);
		}
		
		// Made public so that the Document can use
		public static function drawO(where:Sprite, left:int, top:int, color:uint=COLOR_O):void {
			where.graphics.beginFill(color, 0.5);
			where.graphics.lineStyle(1, color);
			where.graphics.moveTo(left + 2, top + 2);
			where.graphics.lineTo(left + S - 2, top + 2);
			where.graphics.lineTo(left + S - 2, top + S - 2);
			where.graphics.lineTo(left + 2, top + S - 2);
			where.graphics.endFill();
		}
		
		// Made public so that the Document can use
		public static function drawX(where:Sprite, left:int, top:int, color:uint=COLOR_X):void {			
			where.graphics.beginFill(color, 0.5);
			where.graphics.lineStyle(1, color);
			where.graphics.moveTo(left + 2, top + 2);
			where.graphics.lineTo(left + S - 2, top + 2);
			where.graphics.lineTo(left + S - 2, top + S - 2);
			where.graphics.lineTo(left + 2, top + S - 2);
			where.graphics.endFill();
		}
	}
}
