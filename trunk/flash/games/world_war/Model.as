package {
	import net.web20games.game.Model;
	
	public class Model extends net.web20games.game.Model {
		public static const W:int = 16;
		public static const H:int = 15;
		
		// Piece type
		public static const P_NONE	= 0;
		public static const P_O		= 1;
		public static const P_X		= 2;
		public static const P_S		= 3;
		
		public static const F_UP	= 10;
		public static const F_LEFT	= 11;
		public static const F_RIGHT	= 12;
		public static const F_DOWN	= 13;
		public static const S_LEFT	= 14;
		public static const S_RIGHT = 15;
		public static const T_LEFT	= 16;
		public static const T_RIGHT	= 17;
		
		// [row][column]
		public var board:Array;
		public var board1:Array;
		public var board2:Array;
		
		// The document will read this		
		public var lastR:int;
		public var lastC:int;
		public var lastPiece:int = -1 ;
		public var highLightR:int;
		public var highLightC:int;
		public var highLightPiece:int = -1; // No move has done
		public var count:int = 0;
		public var explode:Boolean;
		
		// Flight position and direction.
		public var f1X:int	= -1;
		public var f1Y:int	= -1;
		public var dir1:int	= F_UP;
		
		public var f2X:int	= -1;
		public var f2Y:int	= -1;
		public var dir2:int	= F_UP;
		
		public function Model(baseConfig:Object, extendedConfig:Object):void {
			super(baseConfig, extendedConfig);
			
			// Clear the board.
			board = new Array(H);
			board1 = new Array(H);
			board2 = new Array(H);
			for (var r:int = 0; r < H; r++) {
				board[r] = new Array(W);
				board1[r] = new Array(W);
				board2[r] = new Array(W);
				for (var c:int = 0; c < W; c++) {
					board[r][c] = P_NONE;
					board1[r][c] = P_NONE;
					board2[r][c] = P_NONE;
				}
			}
			f1X = -1;
			f1Y = -1;
			f2X = -1;
			f2Y = -1;
		}		
		
		public override function get baseConfigRanges():Object {
			return {
				nPlayersMin: 2,
				nPlayersMax: 2,
				oneMoveTimeMin: 60,
				oneMoveTimeMax: 300,
				totalTimeMin: 0,
				totalTimeMax: 0,
				batchMode: false};
		}
		
		public override function onMove(index:int, data:Object):int {
			var ret:int;
			try {				
				count ++;
				
				if (count > 3) {
					highLightR		= lastR;
					highLightC		= lastC;
					highLightPiece	= lastPiece;
				}
				
				var a:Array		= data as Array;
				lastR 			= a[0];
				lastC 			= a[1];
				var r:int		= a[0];
				var c:int		= a[1];
				dir1			= a[2];
				dir2			= a[3];
				lastPiece = (index == 0)? P_O : P_X
				
				var d:int	= (index == 0)? dir1: dir2;
				var v:int	= (index == 0)? P_O: P_X;
				var _board:Array = (index ==0)? board1: board2;
				var fX:int	= (index == 0)? f1X: f2X;
								
				if ((count <= 2) && (d == F_UP)) {
					_board[r][c] = v;
					_board[r][c-1] = v;
					_board[r][c+1] = v;
					_board[r-1][c] = v;
					_board[r+1][c] = v;
					_board[r+2][c] = v;
					_board[r+2][c-1] = v;
					_board[r+2][c-2] = v;
					_board[r+2][c+1] = v;
					_board[r+2][c+2] = v;
				}
				if ((count <= 2) && (d == F_DOWN)) {
					_board[r][c] = v;
					_board[r][c-1] = v;
					_board[r][c+1] = v;
					_board[r+1][c] = v;
					_board[r-1][c] = v;
					_board[r-2][c] = v;
					_board[r-2][c-1] = v;
					_board[r-2][c-2] = v;
					_board[r-2][c+1] = v;
					_board[r-2][c+2] = v;
				}
				if ((count <= 2) && (d == F_LEFT)) {
					_board[r][c] = v;
					_board[r-1][c] = v;
					_board[r+1][c] = v;
					_board[r][c-1] = v;
					_board[r][c+1] = v;
					_board[r][c+2] = v;
					_board[r-1][c+2] = v;
					_board[r-2][c+2] = v;
					_board[r+1][c+2] = v;
					_board[r+2][c+2] = v;				
				}
				if ((count <= 2) && (d == F_RIGHT)) {
					_board[r][c] = v;
					_board[r-1][c] = v;
					_board[r+1][c] = v;
					_board[r][c+1] = v;
					_board[r][c-1] = v;
					_board[r][c-2] = v;
					_board[r-1][c-2] = v;
					_board[r-2][c-2] = v;
					_board[r+1][c-2] = v;
					_board[r+2][c-2] = v;				
				}
				if ((count <= 2) && (d == S_RIGHT)) {
					_board[r][c] = v;
					_board[r-1][c] = v;
					_board[r][c-1] = v;
					_board[r][c+1] = v;
					_board[r+1][c] = v;
					_board[r+1][c+1] = v;
					_board[r+1][c-1] = v;
					_board[r+1][c-2] = v;
					_board[r+1][c-3] = v;
					_board[r+2][c] = v;
					_board[r+2][c-1] = v;
					_board[r+2][c-2] = v;
				}
				if ((count <= 2) && (d == S_LEFT)) {
					_board[r][c] = v;
					_board[r-1][c] = v;
					_board[r][c-1] = v;
					_board[r][c+1] = v;
					_board[r+1][c] = v;
					_board[r+1][c-1] = v;
					_board[r+1][c+1] = v;
					_board[r+1][c+2] = v;
					_board[r+1][c+3] = v;
					_board[r+2][c] = v;
					_board[r+2][c+1] = v;
					_board[r+2][c+2] = v;
				}				
				if ((count <= 2) && (d == T_RIGHT)) {
					_board[r][c] = v;
					_board[r-1][c] = v;
					_board[r-1][c+1] = v;
					_board[r-1][c+2] = v;
					_board[r+1][c] = v;
					_board[r+1][c+1] = v;
					_board[r+1][c-1] = v;
					_board[r+1][c-2] = v;
					_board[r+1][c-3] = v;
					_board[r+2][c] = v;
					_board[r+2][c-1] = v;
					_board[r+2][c-2] = v;
				}
				if ((count <= 2) && (d == T_LEFT)) {
					_board[r][c] = v;
					_board[r-1][c] = v;
					_board[r-1][c-1] = v;
					_board[r-1][c-2] = v;
					_board[r+1][c] = v;
					_board[r+1][c-1] = v;
					_board[r+1][c+1] = v;
					_board[r+1][c+2] = v;
					_board[r+1][c+3] = v;
					_board[r+2][c] = v;
					_board[r+2][c+1] = v;
					_board[r+2][c+2] = v;
				}
				
				if ((f1X == -1) && (index == 0)) {
					f1X = c;
					f1Y = r;
					ret = 1;
				}
				if ((f2X == -1) && (index > 0)) {
					f2X = c;
					f2Y = r;
					ret = 0;
				}				
				
				if (count > 2) {					
					_board = (index ==0)? board2: board1;
					explode = false;
					if (_board[lastR][lastC] != P_NONE)
						explode = true;
					_board[lastR][lastC] = P_S;
					ret = computeResult(data);
				}
			} catch (e:Error) {
				trace(e);
				updateGameResult(index, LOST);
				updateGameResult(1 - index, WON);
				ret = GAME_OVER;
			}
			return ret;
		}
		
		// Someone won or Draw or Continue playing.
		private function computeResult(data:Object):int {
			var ret:int;
			var won_1:Boolean = true;
			var won_2:Boolean = true;
			
			for (var i:int = 0; i < H; i++)
				for (var j:int = 0; j < W; j++) {
					if ((board1[i][j] != P_NONE) && (board1[i][j] != P_S))
						won_2 = false;
					if ((board2[i][j] != P_NONE) && (board2[i][j] != P_S))
						won_1 = false;
				}
					
			if (won_1) {
				updateGameResult(0, WON);
				updateGameResult(1, LOST);
				return GAME_OVER;
			}			

			if (won_2) {
				updateGameResult(0, LOST);
				updateGameResult(1, WON);
				return GAME_OVER;
			}			
			
			/*if (drew) {
				updateGameResult(0, DREW);
				updateGameResult(1, DREW);
				return GAME_OVER;
			}*/
			
			// Noone won. Continue playing
			ret = 1 - actionResult;
			return ret;
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
