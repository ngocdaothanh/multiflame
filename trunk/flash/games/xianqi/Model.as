package {
	import net.web20games.game.Model;

	public class Model extends net.web20games.game.Model {
		// Board size = 9x10.
		public static const W = 9;
		public static const H = 10;
		
		// board[row][column].
		public var board:Array;
		
		// Pieces.
		public static const EMPTY		= 0;
		
		public static const BSOLDIER1	= 1;
		public static const BSOLDIER2	= 2;
		public static const BSOLDIER3	= 3;
		public static const BSOLDIER4	= 4;
		public static const BSOLDIER5	= 5;
		
		public static const RSOLDIER1	= 6;
		public static const RSOLDIER2	= 7;
		public static const RSOLDIER3	= 8;
		public static const RSOLDIER4	= 9;
		public static const RSOLDIER5	= 10;
		
		public static const BCANNON1	= 11;
		public static const BCANNON2	= 12;
		public static const RCANNON1	= 13;
		public static const RCANNON2	= 14;
		
		public static const BADVISOR1	= 15;
		public static const BADVISOR2	= 16;
		public static const RADVISOR1	= 17;
		public static const RADVISOR2	= 18;
		
		public static const BELEPHANT1	= 19;
		public static const BELEPHANT2	= 20;
		public static const RELEPHANT1	= 21;
		public static const RELEPHANT2	= 22;
		
		public static const BHORSE1		= 23;
		public static const BHORSE2		= 24;
		public static const RHORSE1		= 25;
		public static const RHORSE2		= 26;
		
		public static const BROOK1		= 27;
		public static const BROOK2		= 28;
		public static const RROOK1		= 29;
		public static const RROOK2		= 30;
		
		public static const BGENERAL	= 31;
		public static const RGENERAL	= 32;
				
		// For update board.
		public var chosenPiece:int 		= -1; // Not choose yet.
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
			board[0] = [BROOK1,		BHORSE1,	BELEPHANT1,	BADVISOR1,	BGENERAL,	BADVISOR2,	BELEPHANT2,	BHORSE2,	BROOK2];
			board[1] = [EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY];
			board[2] = [EMPTY,		BCANNON1,	EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		BCANNON2,	EMPTY];
			board[3] = [BSOLDIER1,	EMPTY,		BSOLDIER2,	EMPTY,		BSOLDIER3,	EMPTY,		BSOLDIER4,	EMPTY,		BSOLDIER5];
			board[4] = [EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY];
			board[5] = [EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY];
			board[6] = [RSOLDIER1,	EMPTY,		RSOLDIER2,	EMPTY,		RSOLDIER3,	EMPTY,		RSOLDIER4,	EMPTY,		RSOLDIER5];
			board[7] = [EMPTY,		RCANNON1,	EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		RCANNON2,	EMPTY];
			board[8] = [EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY,		EMPTY];
			board[9] = [RROOK1,		RHORSE1,	RELEPHANT1,	RADVISOR1,	RGENERAL,	RADVISOR2,	RELEPHANT2,	RHORSE2,	RROOK2];					
		}
		
		// --------------------------------------------------------------------------
		
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
			var blackGeneral:Boolean = false;
			var redGeneral:Boolean	 = false;
			
			// Which general is still alive?
			for (var i:int = 0; i < H; i++)
				for (var j:int = 0; j < W; j++) {
					if (board[i][j] == BGENERAL)
						blackGeneral = true;
					if (board[i][j] == RGENERAL)
						redGeneral	 = true;
				}
				
			// Red won.
			if (!blackGeneral) {
				updateGameResult(0, WON);
				updateGameResult(1, LOST);
				return GAME_OVER;
			}
			
			// Black won.
			if (!redGeneral) {			
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
