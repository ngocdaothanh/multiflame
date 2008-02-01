
package {
	import flash.display.Sprite;
	import flash.events.MouseEvent;
	import flash.text.TextField;

	import net.web20games.game.IDocument;
	import net.web20games.game.IContainer;
	import net.web20games.game.Model;

	public class Document extends Sprite implements IDocument {		
		private var _container:IContainer;
		private var _moveEnabled:Boolean;
		private var _playNicks0:Array;
		private var _indexMe:int;	
		
		public static var	pieceName:Array = new Array();
		public static const	S = 50; //[pixel]
		
		public function Document():void {
			//player1Nick.htmlText = "";
			//player2Nick.htmlText = "";		
			player1Nick.text = "";
			player2Nick.text = "";		
			
			for (var i:int = 0; i < this.numChildren; i++) {
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
			return {modelClass: Model, IntroSprite: new IntroSprite(container)};
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
			pieceName["bSoldier1"]	= Model.BSOLDIER1;
			pieceName["bSoldier2"]	= Model.BSOLDIER2;
			pieceName["bSoldier3"]	= Model.BSOLDIER3;
			pieceName["bSoldier4"]	= Model.BSOLDIER4;
			pieceName["bSoldier5"]	= Model.BSOLDIER5;
			
			pieceName["rSoldier1"]	= Model.RSOLDIER1;
			pieceName["rSoldier2"]	= Model.RSOLDIER2;
			pieceName["rSoldier3"]	= Model.RSOLDIER3;
			pieceName["rSoldier4"]	= Model.RSOLDIER4;
			pieceName["rSoldier5"]	= Model.RSOLDIER5;
			
			pieceName["bCannon1"]	= Model.BCANNON1;
			pieceName["bCannon2"]	= Model.BCANNON2;
			pieceName["rCannon1"]	= Model.RCANNON1;
			pieceName["rCannon2"]	= Model.RCANNON2;

			pieceName["bRook1"]		= Model.BROOK1;
			pieceName["bRook2"]		= Model.BROOK2;
			pieceName["rRook1"]		= Model.RROOK1;
			pieceName["rRook2"]		= Model.RROOK2;
			
			pieceName["bHorse1"]	= Model.BHORSE1;
			pieceName["bHorse2"]	= Model.BHORSE2;
			pieceName["rHorse1"] 	= Model.RHORSE1;
			pieceName["rHorse2"]	= Model.RHORSE2;
			
			pieceName["bElephant1"]	= Model.BELEPHANT1;
			pieceName["bElephant2"]	= Model.BELEPHANT2;
			pieceName["rElephant1"]	= Model.RELEPHANT1;
			pieceName["rElephant2"]	= Model.RELEPHANT2;
			
			pieceName["bAdvisor1"]	= Model.BADVISOR1;
			pieceName["bAdvisor2"]	= Model.BADVISOR2;
			pieceName["rAdvisor1"]	= Model.RADVISOR1;
			pieceName["rAdvisor2"]	= Model.RADVISOR2;
			
			pieceName["bGeneral"]	= Model.BGENERAL;
			pieceName["rGeneral"]	= Model.RGENERAL;	
			
			// Draw board.
			var m:Model = _container.model as Model;
			for (var k:int = 0; k < this.numChildren ; k++) {
				var mc = this.getChildAt(k);
				for (var i:int = 0; i < Model.H; i++)
					for (var j:int = 0; j < Model.W; j++) {
						if (pieceName[mc.name] == m.board[i][j]) {							
							if (indexMe == 0) {
								mc.x = (j + 1) * S;
								mc.y = (i + 0.5) * S; 
							}
							if (indexMe > 0) {
								mc.x = (bC(j) + 1) * S;
								mc.y = (bR(i) + 0.5) * S; 
							}
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
						mc.x = (m.chosenDestC + 1) * S;
						mc.y = (m.chosenDestR + 0.5) * S;
						Dest.x = mc.x;
						Dest.y = mc.y;
						Src.x  = (m._startC + 1) * S;
						Src.y  = (m._startR + 0.5) * S;
					}
					if (_indexMe > 0) {
						mc.x = (bC(m.chosenDestC) + 1) * S;
						mc.y = (bR(m.chosenDestR) + 0.5) * S;
						Dest.x = mc.x;
						Dest.y = mc.y;
						Src.x  = (bC(m._startC) + 1) * S;
						Src.y  = (bR(m._startR) + 0.5) * S;
					}
					var nc = mc;
				}
			}
			
			// Delete killed piece.
			for (i=0; i < this.numChildren ; i++) {
				mc = this.getChildAt(i);
				if (mc.hitTestObject(nc) && (!(mc is Board)) && (mc != nc) && (mc.name != "player1Nick") && (mc.name != "player2Nick") && (mc.name != "Dest") && (mc.name != "Src"))
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
			
			// Choose your piece: Highlight piece and update Model.chosenPiece variable.
			var m:Model = _container.model as Model;
			var t = event.target;
			if (((m.actionResult == 0) && (t.name.charAt(0) == "r")) ||
				((m.actionResult == 1) && (t.name.charAt(0) == "b"))) {				
				m.chosenPiece	 = pieceName[t.name];
				m.chosenPieceName= t.name;
				H.x	= t.x;
				H.y	= t.y;
			} else { // Choose enemy piece.
				if (m.chosenPiece == -1) 
					return;
					
				var i, j:int;
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
			var r:int = event.localY/S;
			var c:int = event.localX/S;
			if (event.localX > (c*S + S/2))
				c++;
			if (event.localY > (r*S + S/2))
				r++;
			if (_indexMe > 0) {
				r = bR(r);
				c = bC(c);
			}
			
			// Cannot move to friendly piece's position.
			if (m.board[r][c] != Model.EMPTY) {
				if ((_indexMe == 0) && (!isBlack(r, c)))
					return;			
				if ((_indexMe > 0) && (isBlack(r, c)))
					return;
			}
				
			// Validate the move.			
			if (validateMove(r, c)) {
				var data:Array = new Array(r, c, m.chosenPiece, m.chosenPieceName, m._startR, m._startC);
				_container.move(data);
			} else {
				return;
			}
		}
		
		// Determine if player made a legal move to destination point(r, c).
		private function validateMove(r:int, c:int):Boolean {
			// Start point of chosen piece(startR, startC).
			var m:Model = _container.model as Model;
			var i, j:int;
			var startR, startC:int;
			for (i = 0; i < Model.H; i++) {
				for (j = 0; j < Model.W; j++) {					
					if (m.board[i][j] == m.chosenPiece) {
						startR = i;
						startC = j;
					}
				}
			}
			m._startR = startR;
			m._startC = startC;
			
			// Validate a black soldier move.
			if ((m.chosenPiece >= pieceName["bSoldier1"]) && (m.chosenPiece <= 5)) {
				if ((c == startC) && (r - startR == 1))
					return true;					
				if 	((startR >= 5) && (r == startR) && (Math.abs(startC-c) == 1))
					return true;
			}
			
			// Validate a red soldier move.
			if ((m.chosenPiece >= 6) && (m.chosenPiece <= 10)) {
				if ((c == startC) && (startR - r == 1))
					return true;
				if 	((startR <= 4) && (r == startR) && (Math.abs(startC-c) == 1))
					return true;
			}
			
			// Validate both black & red rook & cannon move.
			var test:int = 0;
			if (((m.chosenPiece >= 11) && (m.chosenPiece <= 14)) ||
				((m.chosenPiece >= 27) && (m.chosenPiece <= 30))){
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
					if (test == 0)
						return true;
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
					if (test == 0)
						return true;
				}
				
				// Black cannon eat red pieces.
				if ((test == 1) && (m.chosenPiece >= 11) && (m.chosenPiece <= 12)) {
					for (i = 0; i < this.numChildren ; i++) {
						var mc = this.getChildAt(i);
						if ((mc.name.charAt(0) == "r") && (m.board[r][c] == pieceName[mc.name]))
							return true;
					}
				}
				
				// Red cannon eat black pieces.
				if ((test == 1) && (m.chosenPiece >= 13) && (m.chosenPiece <= 14)) {
					for (i = 0; i < this.numChildren ; i++) {
						mc = this.getChildAt(i);
						if ((mc.name.charAt(0) == "b") && (m.board[r][c] == pieceName[mc.name]))
							return true;
					}
				}
			}
			
			// Validate a black general move.
			if (m.chosenPiece == 31) {
				if ((c >= 3) && (c <= 5) && (r >= 0) && (r <= 2)) {
					if ((c == startC) && (Math.abs(startR - r) == 1))
						return true;
					if ((r == startR) && (Math.abs(startC - c) == 1))
						return true;
				}
				// General face.
				if ((m.board[r][c] == Model.RGENERAL) && (startC == c)) {
					var f2f:Boolean = true;
					for (i = r; i < startR; i++)
						if (board[i][c] != Model.EMPTY)
							f2f = false;
					if (f2f)
						return true;
				}					
			}			
			
			// Validate a red general move.			
			if (m.chosenPiece == 32) {
				if ((c >= 3) && (c <= 5) && (r >= 7) && (r <= 9)) {
					if ((c == startC) && (Math.abs(startR - r) == 1))
						return true;
					if ((r == startR) && (Math.abs(startC - c) == 1))
						return true;
				}
				// General face.
				if ((m.board[r][c] == Model.BGENERAL) && (startC == c)) {
					f2f = true;
					for (i = startR; i < r; i++)
						if (board[i][c] != Model.EMPTY)
							f2f = false;
					if (f2f)
						return true;
				}			
			}
			
			// Validate a black advisor move.
			if ((m.chosenPiece == 15) || (m.chosenPiece == 16)) {
				if ((r == 1) && (c == 4) && (startR != 1) && (startC != 4))
					return true;
				if (((r == 0) && (c == 3)) || ((r == 0) && (c == 5)) ||
					((r == 2) && (c == 3)) || ((r == 2) && (c == 5)))
					if ((startR == 1) && (startC == 4))
						return true;
			}
			
			// Validate a red advisor move.
			if ((m.chosenPiece == 17) || (m.chosenPiece == 18)) {
				if ((r == 8) && (c == 4) && (startR != 8) && (startC != 4))
					return true;
				if (((r == 7) && (c == 3)) || ((r == 7) && (c == 5)) ||
					((r == 9) && (c == 3)) || ((r == 9) && (c == 5)))
					if ((startR == 8) && (startC == 4))
						return true;
			}
			
			// Validate an elephant move.
			if ((m.chosenPiece >= 19) && (m.chosenPiece <= 22)) {
				if ((Math.abs(startR - r) == Math.abs(startC - c)) &&  (Math.abs(startC - c) == 2)) {
					var obstacle:Boolean = false;
					i = 1;
					j = 1;
					if (startC > c)
						i = -1;
					if (startR > r)
						j = -1;

					if (m.board[startR + j][startC + i] != Model.EMPTY) {
							obstacle = true;							
					}
					
					if (!obstacle) {
						if (((m.chosenPiece == 19) || (m.chosenPiece == 20)) && (r <=4))
							return true;
						if (((m.chosenPiece == 21) || (m.chosenPiece == 22)) && (r >=5))
							return true;
					}
				}
			}			
			
			// Validate a horse move.
			if ((m.chosenPiece >= 23) && (m.chosenPiece <= 26)) {
				if ((c - startC == 1) && (startR - r == 2) && (m.board[startR - 1][startC] == Model.EMPTY))
					return true;
				if ((c - startC == 1) && (r - startR == 2) && (m.board[startR + 1][startC] == Model.EMPTY))
					return true;
				if ((startC - c == 1) && (startR - r == 2) && (m.board[startR - 1][startC] == Model.EMPTY))
					return true;
				if ((startC - c == 1) && (r - startR == 2) && (m.board[startR + 1][startC] == Model.EMPTY))
					return true;

				if ((c - startC == 2) && (startR - r == 1) && (m.board[startR][startC + 1] == Model.EMPTY))
					return true;
				if ((c - startC == 2) && (r - startR == 1) && (m.board[startR][startC + 1] == Model.EMPTY))
					return true;
				if ((startC - c == 2) && (startR - r == 1) && (m.board[startR][startC - 1] == Model.EMPTY))
					return true;
				if ((startC - c == 2) && (r - startR == 1) && (m.board[startR][startC - 1] == Model.EMPTY))
					return true;				
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
		
		// Return true if black.
		private function isBlack(r:int, c:int):Boolean {
			var m:Model = _container.model as Model;
			var i:int		 = m.board[r][c];
			if ((i >= Model.BSOLDIER1) && (i <= Model.BSOLDIER5))
				return true;
			if ((i == Model.BCANNON1) || (i == Model.BCANNON2))
				return true;
			if ((i == Model.BADVISOR1) || (i == Model.BADVISOR2))
				return true;
			if ((i == Model.BELEPHANT1) || (i == Model.BELEPHANT2))
				return true;
			if ((i == Model.BHORSE1) || (i == Model.BHORSE2))
				return true;
			if ((i == Model.BROOK1) || (i == Model.BROOK2))
				return true;
			if (i == Model.BGENERAL)
				return true;			
			return false;
		}
		
		// Return true if red.
		private function isRed(r:int, c:int):Boolean {
			var m:Model = _container.model as Model;
			if ((!isBlack(r, c)) && (m.board[r][c] != Model.EMPTY))
				return true;
			return false;
		}
	} // End of class.
} // End of package.
