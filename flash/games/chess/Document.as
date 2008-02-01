package {
	import flash.display.Sprite;
	import flash.events.MouseEvent;
	import flash.text.TextField;

	import net.web20games.game.IDocument;
	import net.web20games.game.IContainer;
	import net.web20games.game.Model;

	public class Document extends Sprite implements IDocument {
		// Must-have variables.
		private var _container:IContainer;
		private var _moveEnabled:Boolean;
		private var _playNicks0:Array;
		private var _indexMe:int;
		
		public static var	pieceName:Array = new Array();
		public static const	S = 50; //[pixel] = sqaure size
		public static const B = 8; //[pixel] = border
		
		// Be able for caslting?
		public var wCastling:Boolean = true;
		public var bCastling:Boolean = true;
		
		// Promoted pawn.
		public var promotedPawn:Array = new Array();
		public var chosePromotedPawn:Boolean = false;
		
		public function Document():void {
			//player1Nick.htmlText = "";
			//player2Nick.htmlText = "";	
			player1Nick.text = "";
			player2Nick.text = "";	

			for (var i:int = 0; i < this.numChildren ; i++) {
				var mc = this.getChildAt(i);
				if (mc is Board) {
					mc.addEventListener(MouseEvent.CLICK, onBoardClick);
				} else {
					mc.addEventListener(MouseEvent.CLICK, onPieceClick);
				}
			}
		}		

		public function setContainer(container:IContainer):Object {
			_container = container;
			return {modelClass: Model, introSprite: new IntroSprite(container)};
		}

		public function enableMove(moveEnabled:Boolean):void {
			_moveEnabled = moveEnabled;
		}

		// --------------------------------------------------------------------------

		public function onInit(playNicks0:Array, indexMe:int):void {
			_indexMe		 = indexMe;
			
			// Display nicks.
			if (_indexMe == 0) {
				//player1Nick.htmlText = _container.playerLink(playNicks0[0]);
				//player2Nick.htmlText = _container.playerLink(playNicks0[1]);
				player1Nick.text = playNicks0[0];
				player2Nick.text = playNicks0[1];
			}
			if (_indexMe > 0) {
				//player1Nick.htmlText = _container.playerLink(playNicks0[0]);
				//player2Nick.htmlText = _container.playerLink(playNicks0[1]);
				player1Nick.text = playNicks0[0];
				player2Nick.text = playNicks0[1];
				var temp = player1Nick.textColor;
				player1Nick.textColor = player2Nick.textColor;
				player2Nick.textColor = temp;				
			}
			
			// Hide the highlight piece.
			H.x = -666;
			Dest.x = -666;
			Src.x = -666;
			
			// Initialize pieceName array.
			pieceName["bPawn1"]	= Model.BPAWN1;
			pieceName["bPawn2"]	= Model.BPAWN2;
			pieceName["bPawn3"]	= Model.BPAWN3;
			pieceName["bPawn4"]	= Model.BPAWN4;
			pieceName["bPawn5"]	= Model.BPAWN5;
			pieceName["bPawn6"]	= Model.BPAWN6;
			pieceName["bPawn7"]	= Model.BPAWN7;
			pieceName["bPawn8"]	= Model.BPAWN8;
			
			pieceName["wPawn1"]	= Model.WPAWN1;
			pieceName["wPawn2"]	= Model.WPAWN2;
			pieceName["wPawn3"]	= Model.WPAWN3;
			pieceName["wPawn4"]	= Model.WPAWN4;
			pieceName["wPawn5"]	= Model.WPAWN5;
			pieceName["wPawn6"]	= Model.WPAWN6;
			pieceName["wPawn7"]	= Model.WPAWN7;
			pieceName["wPawn8"]	= Model.WPAWN8;
			
			pieceName["bRook1"]	= Model.BROOK1;
			pieceName["bRook2"]	= Model.BROOK2;
			pieceName["wRook1"]	= Model.WROOK1;
			pieceName["wRook2"]	= Model.WROOK2;
			
			pieceName["bKnight1"] = Model.BKNIGHT1;
			pieceName["bKnight2"] = Model.BKNIGHT2;
			pieceName["wKnight1"] = Model.WKNIGHT1;
			pieceName["wKnight2"] = Model.WKNIGHT2;
			
			pieceName["bBishop1"] = Model.BBISHOP1;
			pieceName["bBishop2"] = Model.BBISHOP2;
			pieceName["wBishop1"] = Model.WBISHOP1;
			pieceName["wBishop2"] = Model.WBISHOP2;
			
			pieceName["bQueen"]	= Model.BQUEEN;
			pieceName["wQueen"]	= Model.WQUEEN;
			
			pieceName["bKing"]	= Model.BKING;
			pieceName["wKing"]	= Model.WKING;			
			
			// Draw board.
			var m:Model = _container.model as Model;
			for (var k:int = 0; k < this.numChildren ; k++) {
				var mc = this.getChildAt(k);
				for (var i:int = 0; i < Model.H; i++)
					for (var j:int = 0; j < Model.W; j++)
						if (pieceName[mc.name] == m.board[i][j]) {							
							if (indexMe == 0) {
								mc.x = board.x + B/2 + (j + 0.5) * S;
								mc.y = board.y + B/2 + (i + 0.5) * S; 
							}
							if (indexMe > 0) {
								mc.x = board.x + B/2 + (bC(j) + 0.5) * S;
								mc.y = board.y + B/2 + (bR(i) + 0.5) * S; 
							}
						}
			}			
		}

		public function onMove():void {
			var m:Model = _container.model as Model;
			
			// Move chosen piece.
			for (var i:int=0; i < this.numChildren ; i++) {
				var mc = this.getChildAt(i);								
				if (mc.name == m.chosenPieceName) {					
					if (_indexMe == 0) {
						mc.x = board.x + B/2 + (m.chosenDestC + 0.5) * S;
						mc.y = board.y + B/2 + (m.chosenDestR + 0.5) * S; 						
						Dest.x = mc.x - S/2;
						Dest.y = mc.y - S/2;
						Src.x  = (m._startC + 1) * S;
						Src.y  = (m._startR + 1) * S;
						// Castling.
						if ((m.chosenPieceName == "wKing") && (m._startC - m.chosenDestC == 2))
							wRook1.x += 150;
						if ((m.chosenPieceName == "bKing") && (m._startC - m.chosenDestC == 2))
							bRook1.x += 150;
						if ((m.chosenPieceName == "wKing") && (m.chosenDestC - m._startC == 2))
							wRook2.x -= 100;
						if ((m.chosenPieceName == "bKing") && (m.chosenDestC - m._startC == 2))
							bRook2.x -= 100;
					}
					if (_indexMe > 0) {
						mc.x = board.x + B/2 + (bC(m.chosenDestC) + 0.5) * S;
						mc.y = board.y + B/2 + (bR(m.chosenDestR) + 0.5) * S; 						
						Dest.x = mc.x - S/2;
						Dest.y = mc.y - S/2;
						Src.x  = (bC(m._startC) + 1) * S;
						Src.y  = (bR(m._startR) + 1) * S;
						// Castling.
						if ((m.chosenPieceName == "wKing") && (m._startC - m.chosenDestC == 2))
							wRook1.x -= 150;
						if ((m.chosenPieceName == "bKing") && (m._startC - m.chosenDestC == 2))
							bRook1.x -= 150;
						if ((m.chosenPieceName == "wKing") && (m.chosenDestC - m._startC == 2))
							wRook2.x += 100;
						if ((m.chosenPieceName == "bKing") && (m.chosenDestC - m._startC == 2))
							bRook2.x += 100;
					}										
					var nc = mc;
				}
			}			
			
			// Delete killed piece.
			for (i=0; i < this.numChildren ; i++) {
				mc = this.getChildAt(i);
				if (mc.hitTestObject(nc) && (!(mc is Board)) && (mc != nc) && (mc.name != "player1Nick") && (mc.name != "player2Nick")  && (mc.name != "Dest") && (mc.name != "Src"))
					mc.x = -666;	// Hide killed piece.
			}
			
			// Reset chosen piece.
			m.chosenPiece = -1;
			H.x = -666;				
		}
		
		public function onMoveBatch():void {
		}

		public function onLeave():void {
		}

		public function onTimeout():void {
		}

		// --------------------------------------------------------------------------
		
		private function onPieceClick(event:MouseEvent):void {
			// If not your turn, nothing happens.
			if (!_moveEnabled) {
				return;
			}
			var i, j:int;
			
			// Choose your piece: Highlight piece and update Model.chosenPiece variable.
			var m:Model = _container.model as Model;
			var t = event.target;			
			if (((m.actionResult == 0) && (t.name.charAt(0) == "w")) ||
				((m.actionResult == 1) && (t.name.charAt(0) == "b"))) {				
				m.chosenPiece	 = pieceName[t.name];
				m.chosenPieceName= t.name;
				H.x	= t.x - S/2;
				H.y	= t.y - S/2;
				chosePromotedPawn = false;
				for (i = 0; i < promotedPawn.length; i++)
					if (pieceName[t.name] == promotedPawn[i])
						chosePromotedPawn = true;
			} else { // Choose enemy piece.
				if (m.chosenPiece == -1) 
					return;				
				
				var chosenR, chosenC:int;
				for (i = 0; i < Model.H; i++) {
					for (j = 0; j < Model.W; j++) {					
						if (m.board[i][j] == pieceName[t.name]) {
							chosenR = i;
							chosenC = j;
						}
					}
				}
				
				// Validate the move.
				if (validateMove(chosenR, chosenC)) {
					var data:Array = new Array(chosenR, chosenC, m.chosenPiece, m.chosenPieceName, m._startR, m._startC);					
					_container.move(data);
				} else {
					return;
				}
			}
		}		
		
		private function onBoardClick(event:MouseEvent):void {
			// If not your turn, nothing happens.
			if (!_moveEnabled) {
				return;
			}
			
			var m:Model = _container.model as Model;
			// If you didn't choose a piece, choose one first.
			if (m.chosenPiece < 0) {
				return;
			}			
			
			// Demetermine user-clicked point(r, c).
			var r:int = (event.localY - B/2)/S;
			var c:int = (event.localX - B/2)/S;
			if (_indexMe > 0) {
				r = bR(r);
				c = bC(c);
			}			
			
			// Cannot move to friendly piece's position.
			if (m.board[r][c] != Model.EMPTY) {
				if ((_indexMe == 0) && (isWhite(r, c)))
					return;			
				if ((_indexMe > 0) && (isBlack(r, c)))
					return;
			}
			
			// Castling.
			if ((m.chosenPieceName == "wKing") && (m.board[7][4] == pieceName["wKing"]) && (r == 7) && (c == 2))
				if (dangerW(7, 2) || dangerW(7, 3) || dangerW(7, 4) || (m.board[7][1] != Model.EMPTY) || (m.board[7][2] != Model.EMPTY) || (m.board[7][3] != Model.EMPTY))
					return;
			if ((m.chosenPieceName == "wKing") && (m.board[7][4] == pieceName["wKing"]) && (r == 7) && (c == 6))
				if (dangerW(7, 6) || dangerW(7, 5) || dangerW(7, 4) || (m.board[7][5] != Model.EMPTY) || (m.board[7][6] != Model.EMPTY))
					return;
			if ((m.chosenPieceName == "bKing") && (m.board[0][4] == pieceName["bKing"]) && (r == 0) && (c == 2))
				if (dangerB(0, 2) || dangerB(0, 3) || dangerB(0, 4) || (m.board[0][1] != Model.EMPTY) || (m.board[0][2] != Model.EMPTY) || (m.board[0][3] != Model.EMPTY))
					return;
			if ((m.chosenPieceName == "bKing") && (m.board[0][4] == pieceName["bKing"]) && (r == 0) && (c == 6))
				if (dangerB(0, 6) || dangerB(0, 5) || dangerB(0, 4) || (m.board[0][5] != Model.EMPTY) || (m.board[0][6] != Model.EMPTY))
					return;
			
			// Validate the move.			
			if (validateMove(r, c)) {
				var data:Array = new Array(r, c, m.chosenPiece, m.chosenPieceName, m._startR, m._startC);
				_container.move(data);
			} else {
				return;
			}
		}
		
		// Determine if player made a legal move to destination point(r, c) (startR = startC = -1)
		// Determine if it's a legal move to destination point(r, c) from (startR, startC)
		private function validateMove(r:int, c:int, startR:int = -1, startC:int = -1, piece:int = -1):Boolean {
			// Start point of chosen piece(startR, startC).
			var m:Model = _container.model as Model;
			var i, j:int;
			var dangerCheck:Boolean = true;
			
			if ((startR == -1) && (startC == -1) && (piece == -1)) {
				dangerCheck = false;
				for (i = 0; i < Model.H; i++)
					for (j = 0; j < Model.W; j++)
						if (m.board[i][j] == m.chosenPiece) {
							startR = i;
							startC = j;
							m._startR = startR;
							m._startC = startC;
							piece = m.chosenPiece;
						}
			}			
			
			// Validate a black pawn move.
			if ((piece >= pieceName["bPawn1"]) && (piece <= pieceName["bPawn8"]) && (!chosePromotedPawn)) {
				// Move forward.
				if ((startC == c) && (m.board[r][c] == Model.EMPTY)) {
					if ((startR == 1) && (r == 3) && (m.board[2][c] == Model.EMPTY))
						return true;
					if (r - startR == 1) {
						if (r == 7)						
							promotedPawn.push(piece);
						return true;
					}
				}
				// Capture enemy.
				if ((Math.abs(startC - c) == 1) && (r - startR == 1) && isWhite(r, c) && (!dangerCheck)) {
					if (r == 7)
						promotedPawn.push(piece);
					return true;
				}
				// Captuarable position.
				if ((Math.abs(startC - c) == 1) && (r - startR == 1) && (m.board[r][c] == Model.EMPTY) && (dangerCheck))
					return true;
			}
			
			// Validate a white pawn move.
			if ((piece >= pieceName["wPawn1"]) && (piece <= pieceName["wPawn8"])) {
				// Move forward.
				if ((startC == c) && (m.board[r][c] == Model.EMPTY)) {
					if ((startR == 6) && (r == 4) && (m.board[5][c] == Model.EMPTY))
						return true;
					if (startR - r == 1) {
						if (r == 0)							
							promotedPawn.push(piece);
						return true;
					}
				}
				// Capture enemy.
				if ((Math.abs(startC - c) == 1) && (startR - r == 1) && isBlack(r, c) && (!dangerCheck)) {
					if (r == 0)						
						promotedPawn.push(piece);
					return true;
				}
				// Captuarable position.
				if ((Math.abs(startC - c) == 1) && (startR - r == 1) && (m.board[r][c] == Model.EMPTY) && (dangerCheck))
					return true;
			}
			
			// Validate a rook or a queen line move.
			var test:int = 0;
			if (((piece >= pieceName["bRook1"]) && (piece <= pieceName["wRook2"])) ||
				(piece == pieceName["bQueen"]) || (piece == pieceName["wQueen"]) || (chosePromotedPawn)) {
				// Side move.
				if (startR == r) {
					if (c < startC)
						for (i = c + 1; i < startC; i++)
							if (m.board[r][i] != Model.EMPTY)
								test++;
					if (c > startC)
						for (i = startC + 1; i < c; i++)
							if (m.board[r][i] != Model.EMPTY)
								test++;
					if (test == 0) {
						if ((piece == pieceName["bRook1"]) || (piece == pieceName["bRook2"]))
							bCastling = false;
						if ((piece == pieceName["wRook1"]) || (piece == pieceName["wRook2"]))
							wCastling = false;
						return true;
					}
				}
				// Straight move.
				if (startC == c) {
					if (r < startR)
						for (i = r + 1; i < startR; i++)
							if (m.board[i][c] != Model.EMPTY)
								test++;
					if (r > startR)
						for (i = startR + 1; i < r; i++)
							if (m.board[i][c] != Model.EMPTY)
								test++;
					if (test == 0) {
						if ((piece == pieceName["bRook1"]) || (piece == pieceName["bRook2"]))
							bCastling = false;
						if ((piece == pieceName["wRook1"]) || (piece == pieceName["wRook2"]))
							wCastling = false;
						return true;
					}
				}
			}
			
			// Validate a bishop move.
			if (((piece >= pieceName["bBishop1"]) && (piece <= pieceName["wBishop2"])) || 
				(piece == pieceName["bQueen"]) || (piece == pieceName["wQueen"]) || (chosePromotedPawn)) {
				if (Math.abs(startR - r) == Math.abs(startC - c)) {
					var obstacle:Boolean = false;
					var dC:int = 1;
					var dR:int = 1;
					if (startC > c)
						dC = -1;
					if (startR > r)
						dR = -1;

					i = startC;
					j = startR;					
					do {
						i += dC;
						j += dR;
						if ((i == c) && (j == r))
							return true;
						if (m.board[j][i] != Model.EMPTY) {
							obstacle = true;							
						}						
					} while ((i + dC != c) && (j + dR != r))
					
					if (!obstacle)
						return true;
				}				
			}
			
			// Validate a knight move.
			if ((piece >= pieceName["bKnight1"]) && (piece <= pieceName["wKnight2"])) {
				if ((Math.abs(startR - r) == 2) && (Math.abs(startC - c) == 1))
					return true;
				if ((Math.abs(startR - r) == 1) && (Math.abs(startC - c) == 2))
					return true;
			}
			
			// Validate a king move.
			if ((piece == pieceName["bKing"]) || (piece == pieceName["wKing"])) {
				// Casual move.
				if ((Math.abs(startR - r) <= 1) && (Math.abs(startC - c) <= 1)) {
					if (piece == pieceName["bKing"])
						bCastling = false;
					if (piece == pieceName["wKing"])
						wCastling = false;
					return true;
				}
				// Castling.
				if ((wCastling) && (Math.abs(startC - c) == 2) && (!dangerCheck)) {
					wCastling = false;
					return true;					
				}
				if ((bCastling) && (Math.abs(startC - c) == 2) && (!dangerCheck)) {					
					bCastling = false;
					return true;					
				}
			}
						
			// Illegal move.
			return false;
		}
		
		// Convert from red coordinate into black coordinate.
		private function bR(i:int):int {
			return ((Model.H -1) - i);
		}
		
		private function bC(i:int):int {
			return ((Model.W -1) - i);
		}
		
		// Return true is black.
		private function isBlack(r:int, c:int):Boolean {
			var m:Model = _container.model as Model;
			var i:int		 = m.board[r][c];
			if ((i >= Model.BPAWN1) && (i <= Model.BPAWN8))
				return true;
			if ((i == Model.BROOK1) || (i == Model.BROOK2))
				return true;
			if ((i == Model.BKNIGHT1) || (i == Model.BKNIGHT2))
				return true;
			if ((i == Model.BBISHOP1) || (i == Model.BBISHOP2))
				return true;
			if ((i == Model.BQUEEN) || (i == Model.BKING))
				return true;			
			return false;
		}
		
		// Return true if white.
		private function isWhite(r:int, c:int):Boolean {
			var m:Model = _container.model as Model;
			if ((!isBlack(r, c)) && (m.board[r][c] != Model.EMPTY))
				return true;
			return false;
		}		
		
		// Determine if a position is dangerous for black.
		private function dangerB(r:int, c:int):Boolean {
			var m:Model = _container.model as Model;
			for (var _r:int = 0; _r < 8; _r++)
				for (var _c:int = 0; _c < 8; _c++) {
					if (isWhite(_r, _c))
						if (validateMove(r, c, _r, _c, m.board[_r][_c]))
							return true;			
				}
			return false;
		}
		
		// Determine if a position is dangerous for white.
		private function dangerW(r:int, c:int):Boolean {
			var m:Model = _container.model as Model;
			for (var _r:int = 0; _r < 8; _r++)
				for (var _c:int = 0; _c < 8; _c++) {
					if (isBlack(_r, _c))
						if (validateMove(r, c, _r, _c, m.board[_r][_c])) {
							player2Nick.text = m.board[_r][_c] + "";
							return true;
						}
				}
			return false;
		}
	}
}