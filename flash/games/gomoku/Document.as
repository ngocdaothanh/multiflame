package {
	import flash.display.Sprite;
	import flash.text.TextField;
	import flash.events.MouseEvent;
	
	import net.web20games.game.IDocument;
	import net.web20games.game.IContainer;
	
	public class Document extends Sprite implements IDocument {
		private var _p1Nick:TextField;
		private var _p2Nick:TextField;
		private var _view:View;
		private var _enabled:Boolean;
		
		private var _container:IContainer;
		
		public function Document():void {			
			View.drawO(this, 100, 5);
			_p1Nick = new TextField();
			_p1Nick.x = 120;
			_p1Nick.y = 3;
			addChild(_p1Nick);
			View.drawX(this, 100, 480);
			_p2Nick = new TextField();
			_p2Nick.x = _p1Nick.x;
			_p2Nick.y = 478;
			addChild(_p2Nick);

			_view = new View();
			_view.x = 3;
			_view.y = 23;
			_view.drawBoard();
			addChild(_view);
			
			_view.addEventListener(MouseEvent.CLICK, onClick);
		}
		
		private function onClick(event:MouseEvent):void {
			if (!_enabled) {
				return;
			}
			var r:int = event.localY/View.S;
			var c:int = event.localX/View.S;
			
			// Check that this position is empty
			var m:Model = _container.model as Model;
			if (m.board[r][c] == Model.P_NONE) {
				var data:Array = new Array(r, c);
				_container.move(data);
			}
		}
		
		// --------------------------------------------------------------------------
		
		public function setContainer(container:IContainer):Object {
			_container = container;
			return {modelClass: Model, introSprite: new IntroSprite(container)};
		}

		public function enableMove(enabled:Boolean):void {
			_enabled = enabled;
		}

		// --------------------------------------------------------------------------

		public function onInit(playNicks0:Array, indexMe:int):void {		
			//_p1Nick.htmlText = _container.playerLink(playNicks0[0]);
			//_p2Nick.htmlText = _container.playerLink(playNicks0[1]);
			_p1Nick.text = playNicks0[0];
			_p2Nick.text = playNicks0[1];
			_view.drawBoard();
		}

		public function onMove():void {
			var m:Model = _container.model as Model;
			if (m.highLightPiece != -1) { 
				_view.drawHighLightPiece(m.lastPiece, m.lastR, m.lastC);
				_view.drawPiece(m.highLightPiece, m.highLightR, m.highLightC);
			} else {
				_view.drawHighLightPiece(m.lastPiece, m.lastR, m.lastC);
			}			
		}
		
		public function onMoveBatch():void {
		}
		
		public function onLeave():void {
		}
		
		public function onTimeout():void {
		}
		
	}
}
