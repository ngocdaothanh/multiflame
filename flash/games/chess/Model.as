package {
	import net.web20games.game.Model;

	public class Model extends net.web20games.game.Model {
		// Board size = 9x10.
		public static const W = 8;
		public static const H = 8;
		
		// board[row][column].
		public var board:Array;
		
		// Pieces.
		public static const EMPTY	= 0;
		
		public static const BPAWN1	= 1;
		public static const BPAWN2	= 2;
		public static const BPAWN3	= 3;
		public static const BPAWN4	= 4;
		public static const BPAWN5	= 5;
		public static const BPAWN6	= 6;
		public static const BPAWN7	= 7;
		public static const BPAWN8	= 8;
		
		public static const WPAWN1	= 9;
		public static const WPAWN2	= 10;
		public static const WPAWN3	= 11;
		public static const WPAWN4	= 12;
		public static const WPAWN5	= 13;
		public static const WPAWN6	= 14;
		public static const WPAWN7	= 15;
		public static const WPAWN8	= 16;
		
		public static const BROOK1	= 17;
		public static const BROOK2	= 18;
		public static const WROOK1	= 19;
		public static const WROOK2	= 20;
		
		public static const BKNIGHT1= 21;
		public static const BKNIGHT2= 22;
		public static const WKNIGHT1= 23;
		public static const WKNIGHT2= 24;
		
		public static const BBISHOP1= 25;
		public static const BBISHOP2= 26;
		public static const WBISHOP1= 27;
		public static const WBISHOP2= 28;
		
		public static const BQUEEN	= 29;
		public static const WQUEEN	= 30;
		
		public static const BKING	= 31;
		public static const WKING	= 32;
		
		// For update board.
		public var chosenPiece:int 	= -1; // Not choose yet.
		public var chosenPieceName:String;
		public var chosenDestR:int;
		public var chosenDestC:int;
		public var _startR:int;
		public var _startC:int;		

		public function Model(baseConfig:Object, extendedConfig:Object):void {
			super(baseConfig, extendedConfig);

			// Initialize piece array.
			board = new Array(H);
			for (var i:int = 0; i < H; i++)
				board[i] = new Array(W);
			board[0] = [BROOK1,	BKNIGHT1,	BBISHOP1,	BQUEEN,	BKING,	BBISHOP2,	BKNIGHT2,	BROOK2];
			board[1] = [BPAWN1,	BPAWN2,		BPAWN3,		BPAWN4,	BPAWN5,	BPAWN6,		BPAWN7,		BPAWN8];
			board[2] = [EMPTY, 	EMPTY,		EMPTY,		EMPTY,	EMPTY,	EMPTY,		EMPTY,		EMPTY];
			board[3] = [EMPTY, 	EMPTY,		EMPTY,		EMPTY,	EMPTY,	EMPTY,		EMPTY,		EMPTY];
			board[4] = [EMPTY, 	EMPTY,		EMPTY,		EMPTY,	EMPTY,	EMPTY,		EMPTY,		EMPTY];
			board[5] = [EMPTY, 	EMPTY,		EMPTY,		EMPTY,	EMPTY,	EMPTY,		EMPTY,		EMPTY];
			board[6] = [WPAWN1,	WPAWN2,		WPAWN3,		WPAWN4,	WPAWN5,	WPAWN6,		WPAWN7,		WPAWN8];
			board[7] = [WROOK1,	WKNIGHT1,	WBISHOP1,	WQUEEN,	WKING,	WBISHOP2,	WKNIGHT2,	WROOK2];			
		}
		
		// --------------------------------------------------------------------------

		public override function get baseConfigRanges():Object {
			return {
				nPlayersMin: 2,
				nPlayersMax: 2,
				oneMoveTimeMin: 60,
				oneMoveTimeMax: 600,
				totalTimeMin: 0,
				totalTimeMax: 0,
				batchMode: false};
		}

		public override function onMove(index:int, data:Object):int {
			var ret:int;
			try {
				var a:Array	= data as Array;
				chosenDestR = a[0];
				chosenDestC = a[1];
				chosenPiece = a[2]
				chosenPieceName = a[3];
				_startR = a[4];
				_startC = a[5];
				
				var iP, jP:int;				
				for (var i:int = 0; i < H; i++)
					for (var j:int = 0; j< W; j++)
						if (board[i][j] == chosenPiece) {
							iP = i;
							jP = j;							
						}						
				board[chosenDestR][chosenDestC]	= chosenPiece;
				board[iP][jP] = EMPTY;
				// Castling.
				if ((chosenPieceName == "wKing") && (_startC - chosenDestC == 2)) {
					board[7][0] = EMPTY;
					board[7][3] = WROOK1;
				}
				if ((chosenPieceName == "wKing") && (chosenDestC - _startC == 2)) {
					board[7][7] = EMPTY;
					board[7][5] = WROOK2;
				}
				if ((chosenPieceName == "bKing") && (_startC - chosenDestC == 2)) {
					board[0][0] = EMPTY;
					board[0][3] = BROOK1;
				}
				if ((chosenPieceName == "bKing") && (chosenDestC - _startC == 2)) {
					board[0][7] = EMPTY;
					board[0][5] = BROOK2;
				}
				
				ret = computeResult(index);
			} catch (e:Error) {
				updateGameResult(index, LOST);
				updateGameResult(1 - index, WON);
				ret = GAME_OVER;
			}
			return ret;
		}

		public override function onLeave(index:int):int {
			updateGameResult(index, LOST);
			updateGameResult(1 - index, WON);
			return GAME_OVER;
		}

		public override function onTimeout():int {
			updateGameResult(_actionResult, LOST);
			updateGameResult(1 - _actionResult, WON);
			return GAME_OVER;
		}

		private function computeResult(index:int):int {			
			var blackKing:Boolean = false;
			var whiteKing:Boolean = false;
			
			// Which king is still alive?
			for (var i:int = 0; i < H; i++)
				for (var j:int = 0; j < W; j++) {
					if (board[i][j] == BKING)
						blackKing = true;
					if (board[i][j] == WKING)
						whiteKing = true;
				}
				
			// Red won.
			if (!blackKing) {
				updateGameResult(0, WON);
				updateGameResult(1, LOST);
				return GAME_OVER;
			}
			
			// Black won.
			if (!whiteKing) {			
				updateGameResult(0, LOST);
				updateGameResult(1, WON);
				return GAME_OVER;
			}			

			// If (Drew) {			
			//	updateGameResult(0, DREW);
			//	updateGameResult(1, DREW);
			//	return GAME_OVER;
			//}

			// Next player.
			return (1 - index);
		}
	}
}
