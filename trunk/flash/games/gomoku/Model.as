package {
	import net.web20games.game.Model;
	
	public class Model extends net.web20games.game.Model {
		public static const W:int = 33;
		public static const H:int = 30;
		
		// Piece type
		public static const P_NONE = 0;
		public static const P_O    = 1;
		public static const P_X    = 2;
		
		// [row][column]
		public var board:Array;
		
		// The document will read this
		public var lastR:int;
		public var lastC:int;
		public var lastPiece:int = -1 ;
		public var highLightR:int;
		public var highLightC:int;
		public var highLightPiece:int = -1; // No move has done
		
		public function Model(baseConfig:Object, extendedConfig:Object):void {
			super(baseConfig, extendedConfig);
			
			// Clear the board
			board = new Array(H);
			for (var r:int = 0; r < H; r++) {
				board[r] = new Array(W);
				for (var c:int = 0; c < W; c++) {
					board[r][c] = P_NONE;
				}
			}
		}
		
		/*public static function get baseConfigRanges():Array {
			return new Array(2, 2, 10, 60, false, 0, 0);
		}*/
		public override function get baseConfigRanges():Object {
			return {
				nPlayersMin: 2,
				nPlayersMax: 2,
				oneMoveTimeMin: 10,
				oneMoveTimeMax: 60,
				totalTimeMin: 0,
				totalTimeMax: 0,
				batchMode: false};
		}
		
		public override function onMove(index:int, data:Object):int {
			var ret:int;
			try {
				var a:Array		= data as Array;
				highLightR		= lastR;
				highLightC		= lastC;
				highLightPiece	= lastPiece;
				
				lastR 			= a[0];
				lastC 			= a[1];				
				if (board[lastR][lastC] != P_NONE) {
					throw new Error();
				}
				lastPiece = (index == 0)? P_O : P_X
				
				board[lastR][lastC] = lastPiece;
				
				//ret = 1 - actionResult;		// Don't care who wins
				ret = computeResult(data);		// Care about winner
			} catch (e:Error) {
				trace(e);
				updateGameResult(index, LOST);
				updateGameResult(1 - index, WON);
				ret = GAME_OVER;
			}
			return ret;
		}
		
		// Someone won or Draw or Continue playing
		private function computeResult(data:Object):int {
			var ret:int;		
			
			// P_0 won?
			if (won(P_O)) {
				updateGameResult(0, WON);
				updateGameResult(1, LOST);
				return GAME_OVER;
			}
			
			// P_X won?
			if (won(P_X)) {
				updateGameResult(0, LOST);
				updateGameResult(1, WON);
				return GAME_OVER;
			}
			
			// Drew?
			var count:int = 0;
			for (var i:int = 0; i < H; i++) {
				for (var j:int = 0;j< W;j++) {					
					if (board[i][j] != P_NONE) {
						count++;
					}
				}
			}
			if (count == W*H) {
				updateGameResult(0, DREW);
				updateGameResult(1, DREW);
				return GAME_OVER;
			}
			
			// Noone won. Continue playing
			ret = 1 - actionResult;
			return ret;
		}			
		
		// Check if P_O or P_X won
		private function won(pieceType:int):Boolean {			
			var i:int;
			var j:int;
			var num:int;

			// Check vertical
			num	= 0;
			i	= lastR;			
			while ((i>=0) && (board[i][lastC] == pieceType)) {	// Up
				num++;
				i--;				
			}
			i	= lastR+1;			
			while ((i<H) && (board[i][lastC] == pieceType)) {	// Down
				num++;
				i++;				
			}
			if (num >= 5)
				return true;
				
			// Check horizontal
			num	= 0;
			i	= lastC;
			while ((i>=0) && (board[lastR][i] == pieceType)) {
				num++;
				i--;				
			}
			i	= lastC+1;			
			while ((i<W) && (board[lastR][i] == pieceType)) {
				num++;
				i++;				
			}
			if (num >= 5)
				return true;				
				
			// Check slash (/)
			num	= 0;
			i	= lastR;			
			j	= lastC;
			while ((j<W) && (i>=0) && (board[i][j] == pieceType)) {
				num++;
				i--;
				j++;
			}
			i	= lastR+1;			
			j	= lastC-1;
			while ((i<H) && (j>=0) && (board[i][j] == pieceType)) {
				num++;
				i++;
				j--;
			}
			if (num >= 5)
				return true;

			// Check back-slash (\)
			num	= 0;
			i	= lastR;			
			j	= lastC;
			while ((i>=0) && (j>=0) && (board[i][j] == pieceType)) {
				num++;
				i--;
				j--;
			}
			i	= lastR+1;			
			j	= lastC+1;
			while ((i<H) && (j<W) && (board[i][j] == pieceType)) {
				num++;
				i++;
				j++;
			}
			if (num >= 5)
				return true;
						
			return false;
		}		

		public override function onLeave(index:int):int {
			updateGameResult(index, LOST);
			updateGameResult(1 - index, WON);
			return GAME_OVER;
		}

		public override function onTimeout():int {
			updateGameResult(actionResult, LOST);
			updateGameResult(1 - actionResult, WON);
			return GAME_OVER;
		}		
	}
}
