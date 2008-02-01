package {
	import flash.display.Sprite;
	import flash.text.TextField;
	import flash.events.MouseEvent;
	
	import net.web20games.game.IDocument;
	import net.web20games.game.IContainer;
	
	public class Document extends Sprite implements IDocument {
		private var player1Nick:TextField;
		private var player2Nick:TextField;
		private var _view:View;
		private var m:Model;
		private var _enabled:Boolean;
		private var _index:int;
		private var chose:Boolean = false;
		
		private var _container:IContainer;
		
		public function Document():void {			
			// Nicks.
			player1Nick = new TextField();
			player1Nick.x = 120;
			player1Nick.y = 3;
			addChild(player1Nick);
			player2Nick = new TextField();
			player2Nick.x = player1Nick.x;
			player2Nick.y = 478;
			addChild(player2Nick);
			
			// FlightChoser.
			flightChoser.x = -666;
		}
		
		private function onClick(event:MouseEvent):void {
			// If not your turn, wait.
			if (!_enabled) {
				return;
			}
			
			if ((_index == 1) && (m.f1X == -1))
				return;
			
			var r:int	= event.localY/View.S;			
			var c:int	= event.localX/View.S;		
			
			// Check that this position is empty.
			if (((m.board1[r][c] != Model.P_S) && (_index == 1)) || ((m.board2[r][c] != Model.P_S) && (_index == 0))) {
				var data:Array = new Array(r, c, m.dir1, m.dir2);
				_container.move(data);
			}
		}
		
		// --------------------------------------------------------------------------
		
		public function setContainer(container:IContainer):Object {
			_container = container;
			return {modelClass: Model, IntroSprite: new IntroSprite(container)};
		}

		public function enableMove(enabled:Boolean):void {
			_enabled = enabled;
		}

		// --------------------------------------------------------------------------

		public function onInit(playNicks0:Array, indexMe:int):void {
			m = _container.model as Model;
			_index = indexMe;
			
			// View.
			if (_view != null)
				removeChild(_view);
			_view = new View();
			_view.x = 3;
			_view.y = 23;			
			addChild(_view);
			_view.addEventListener(MouseEvent.CLICK, onClick);			
			_view.addEventListener(MouseEvent.MOUSE_MOVE, choosePosition);
			chose = false;
			
			// Draw nicks.			
			//player1Nick.htmlText = _container.playerLink(playNicks0[0]);
			//player2Nick.htmlText = _container.playerLink(playNicks0[1]);
			player1Nick.text = playNicks0[0];
			player2Nick.text = playNicks0[1];
			
			// Choose flight.
			flightChoser.x = 250;
			flightChoser.upF.addEventListener(MouseEvent.CLICK, upFClicked);
			flightChoser.leftF.addEventListener(MouseEvent.CLICK, leftFClicked);
			flightChoser.rightF.addEventListener(MouseEvent.CLICK, rightFClicked);
			flightChoser.downF.addEventListener(MouseEvent.CLICK, downFClicked);			
			flightChoser.OKBtn.addEventListener(MouseEvent.CLICK, OKBtnClicked);
			flightChoser.leftS.addEventListener(MouseEvent.CLICK, leftSClicked);
			flightChoser.rightS.addEventListener(MouseEvent.CLICK, rightSClicked);
			flightChoser.leftT.addEventListener(MouseEvent.CLICK, leftTClicked);
			flightChoser.rightT.addEventListener(MouseEvent.CLICK, rightTClicked);
		}

		public function onMove():void {
			var d:int	= (_index == 0)? m.dir1: m.dir2;
			var fX:int	= (_index == 0)? m.f1X: m.f2X;
			var fY:int	= (_index == 0)? m.f1Y: m.f2Y;
			var r:int	= m.lastR;
			var c:int	= m.lastC;

			// Click to set a position.
			if ((!chose) && (fX != -1) && (c >= 2) && (c <= 13) && (r >= 1) && (r <= 12) && (d == Model.F_UP)) {
				_view.drawFlight(r, c);
				_view.drawFlight(r, c-1);
				_view.drawFlight(r, c+1);
				_view.drawFlight(r-1, c);
				_view.drawFlight(r+1, c);
				_view.drawFlight(r+2, c);
				_view.drawFlight(r+2, c-1);
				_view.drawFlight(r+2, c-2);
				_view.drawFlight(r+2, c+1);
				_view.drawFlight(r+2, c+2);				
				F_U.x = -666;
				chose = true;
				return;
			}
			if ((!chose) && (fX != -1) && (c >= 2) && (c <= 13) && (r >= 1) && (r <= 12) && (d == Model.F_DOWN)) {
				_view.drawFlight(r, c);
				_view.drawFlight(r, c-1);
				_view.drawFlight(r, c+1);
				_view.drawFlight(r+1, c);
				_view.drawFlight(r-1, c);
				_view.drawFlight(r-2, c);
				_view.drawFlight(r-2, c-1);
				_view.drawFlight(r-2, c-2);
				_view.drawFlight(r-2, c+1);
				_view.drawFlight(r-2, c+2);				
				F_D.x = -666;
				chose = true;
				return;
			}
			if ((!chose) && (fX != -1) && (c >= 1) && (c <= 13) && (r >= 2) && (r <= 12) && (d == Model.F_LEFT)) {
				_view.drawFlight(r, c);
				_view.drawFlight(r-1, c);
				_view.drawFlight(r+1, c);
				_view.drawFlight(r, c-1);
				_view.drawFlight(r, c+1);
				_view.drawFlight(r, c+2);
				_view.drawFlight(r-1, c+2);
				_view.drawFlight(r-2, c+2);
				_view.drawFlight(r+1, c+2);
				_view.drawFlight(r+2, c+2);				
				F_L.x = -666;
				chose = true;
				return;
			}
			if ((!chose) && (fX != -1) && (c >= 2) && (c <= 14) && (r >= 2) && (r <= 12) && (d == Model.F_RIGHT)) {
				_view.drawFlight(r, c);
				_view.drawFlight(r-1, c);
				_view.drawFlight(r+1, c);
				_view.drawFlight(r, c+1);
				_view.drawFlight(r, c-1);
				_view.drawFlight(r, c-2);
				_view.drawFlight(r-1, c-2);
				_view.drawFlight(r-2, c-2);
				_view.drawFlight(r+1, c-2);
				_view.drawFlight(r+2, c-2);				
				F_R.x = -666;
				chose = true;
				return;
			}
			if ((!chose) && (fX != -1) && (c >= 3) && (c <= 14) && (r >= 1) && (r <= 12) && (d == Model.S_RIGHT)) {
				_view.drawFlight(r, c);
				_view.drawFlight(r-1, c);
				_view.drawFlight(r, c-1);
				_view.drawFlight(r, c+1);
				_view.drawFlight(r+1, c);
				_view.drawFlight(r+1, c+1);
				_view.drawFlight(r+1, c-1);
				_view.drawFlight(r+1, c-2);
				_view.drawFlight(r+1, c-3);
				_view.drawFlight(r+2, c);
				_view.drawFlight(r+2, c-1);
				_view.drawFlight(r+2, c-2);				
				S_R.x = -666;
				chose = true;
				return;
			}
			if ((!chose) && (fX != -1) && (c >= 1) && (c <= 12) && (r >= 1) && (r <= 12) && (d == Model.S_LEFT)) {
				_view.drawFlight(r, c);
				_view.drawFlight(r-1, c);
				_view.drawFlight(r, c-1);
				_view.drawFlight(r, c+1);
				_view.drawFlight(r+1, c);
				_view.drawFlight(r+1, c-1);
				_view.drawFlight(r+1, c+1);
				_view.drawFlight(r+1, c+2);
				_view.drawFlight(r+1, c+3);
				_view.drawFlight(r+2, c);
				_view.drawFlight(r+2, c+1);
				_view.drawFlight(r+2, c+2);				
				S_L.x = -666;
				chose = true;
				return;
			}
			if ((!chose) && (fX != -1) && (c >= 3) && (c <= 13) && (r >= 1) && (r <= 12) && (d == Model.T_RIGHT)) {
				_view.drawFlight(r, c);
				_view.drawFlight(r-1, c);
				_view.drawFlight(r-1, c+1);
				_view.drawFlight(r-1, c+2);
				_view.drawFlight(r+1, c);
				_view.drawFlight(r+1, c+1);
				_view.drawFlight(r+1, c-1);
				_view.drawFlight(r+1, c-2);
				_view.drawFlight(r+1, c-3);
				_view.drawFlight(r+2, c);
				_view.drawFlight(r+2, c-1);
				_view.drawFlight(r+2, c-2);
				T_R.x = -666;
				chose = true;
				return;
			}
			if ((!chose) && (fX != -1) && (c >= 2) && (c <= 12) && (r >= 1) && (r <= 12) && (d == Model.T_LEFT)) {
				_view.drawFlight(r, c);
				_view.drawFlight(r-1, c);
				_view.drawFlight(r-1, c-1);
				_view.drawFlight(r-1, c-2);
				_view.drawFlight(r+1, c);
				_view.drawFlight(r+1, c-1);
				_view.drawFlight(r+1, c+1);
				_view.drawFlight(r+1, c+2);
				_view.drawFlight(r+1, c+3);
				_view.drawFlight(r+2, c);
				_view.drawFlight(r+2, c+1);
				_view.drawFlight(r+2, c+2);
				T_L.x = -666;
				chose = true;
				return;
			}
			
			if (m.count > 2) {
				if (m.highLightPiece != -1) {
					_view.drawHighLightPiece(m.lastPiece, m.lastR, m.lastC);
					_view.drawPiece(m.highLightPiece, m.highLightR, m.highLightC);
				} else {
					_view.drawHighLightPiece(m.lastPiece, m.lastR, m.lastC);
				}
				if (m.explode)
					_view.drawExplode(m.lastR, m.lastC);
			}
		}
		
		
		public function onMoveBatch():void {
		}
		
		public function onLeave():void {
		}
		
		public function onTimeout():void {
		}
		
		private function upFClicked(event:MouseEvent):void {
			if (_index == 0)
				m.dir1 = Model.F_UP;
			if (_index > 0)
				m.dir2 = Model.F_UP;
			flightChoser.H.x = -200;
			flightChoser.H.y = -47;
		}
		
		private function leftFClicked(event:MouseEvent):void {
			if (_index == 0)
				m.dir1 = Model.F_LEFT;
			if (_index > 0)
				m.dir2 = Model.F_LEFT;
			flightChoser.H.x = -98;
			flightChoser.H.y = -47;
		}
		
		private function rightFClicked(event:MouseEvent):void {
			if (_index == 0)
				m.dir1 = Model.F_RIGHT;
			if (_index > 0)
				m.dir2 = Model.F_RIGHT;
			flightChoser.H.x = 8;
			flightChoser.H.y = -47;
		}
		
		private function downFClicked(event:MouseEvent):void {
			if (_index == 0)
				m.dir1 = Model.F_DOWN;
			if (_index > 0)
				m.dir2 = Model.F_DOWN;
			flightChoser.H.x = 110;
			flightChoser.H.y = -47;
		}
		
		private function leftSClicked(event:MouseEvent):void {
			if (_index == 0)
				m.dir1 = Model.S_LEFT;
			if (_index > 0)
				m.dir2 = Model.S_LEFT;
			flightChoser.H.x = -197;
			flightChoser.H.y = 60;
		}
		
		private function leftTClicked(event:MouseEvent):void {
			if (_index == 0)
				m.dir1 = Model.T_LEFT;
			if (_index > 0)
				m.dir2 = Model.T_LEFT;
			flightChoser.H.x = -86;
			flightChoser.H.y = 60;
		}
		
		private function rightTClicked(event:MouseEvent):void {
			if (_index == 0)
				m.dir1 = Model.T_RIGHT;
			if (_index > 0)
				m.dir2 = Model.T_RIGHT;
			flightChoser.H.x = 12;
			flightChoser.H.y = 60;
		}
		
		private function rightSClicked(event:MouseEvent):void {
			if (_index == 0)
				m.dir1 = Model.S_RIGHT;
			if (_index > 0)
				m.dir2 = Model.S_RIGHT;
			flightChoser.H.x = 121;
			flightChoser.H.y = 60;
		}
		
		private function removeListeners():void {
			flightChoser.upF.removeEventListener(MouseEvent.CLICK, upFClicked);
			flightChoser.leftF.removeEventListener(MouseEvent.CLICK, leftFClicked);
			flightChoser.rightF.removeEventListener(MouseEvent.CLICK, rightFClicked);
			flightChoser.downF.removeEventListener(MouseEvent.CLICK, downFClicked);			
			flightChoser.OKBtn.removeEventListener(MouseEvent.CLICK, OKBtnClicked);
			flightChoser.leftS.removeEventListener(MouseEvent.CLICK, leftSClicked);
			flightChoser.rightS.removeEventListener(MouseEvent.CLICK, rightSClicked);
			flightChoser.leftT.removeEventListener(MouseEvent.CLICK, leftTClicked);
			flightChoser.rightT.removeEventListener(MouseEvent.CLICK, rightTClicked);
		}
		
		private function OKBtnClicked(event:MouseEvent):void {
			flightChoser.x = -666;
			_view.drawBoard();
		}
		
		private function choosePosition(event:MouseEvent):void {			
			var r:int = mouseX/View.S;
			var c:int = mouseY/View.S;			
			var d:int = (_index == 0)? m.dir1: m.dir2;
			var fX:int	= (_index == 0)? m.f1X: m.f2X;			
			if (fX != -1) {
				_view.removeEventListener(MouseEvent.MOUSE_MOVE, choosePosition);
				return;
			}
			if (d == Model.F_UP) {
				F_U.x = mouseX;
				F_U.y = mouseY;
			}
			if (d == Model.F_DOWN) {
				F_D.x = mouseX;
				F_D.y = mouseY;
			}			
			if (d == Model.F_LEFT) {
				F_L.x = mouseX;
				F_L.y = mouseY;
			}
			if (d == Model.F_RIGHT) {
				F_R.x = mouseX;
				F_R.y = mouseY;
			}
			if (d == Model.S_LEFT) {
				S_L.x = mouseX;
				S_L.y = mouseY;
			}
			if (d == Model.T_LEFT) {
				T_L.x = mouseX;
				T_L.y = mouseY;
			}
			if (d == Model.S_RIGHT) {
				S_R.x = mouseX;
				S_R.y = mouseY;
			}
			if (d == Model.T_RIGHT) {
				T_R.x = mouseX;
				T_R.y = mouseY;
			}			
		}
	}
}