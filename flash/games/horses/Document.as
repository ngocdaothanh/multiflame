package {
	import flash.display.Sprite;
	import flash.display.SimpleButton;
	import flash.events.MouseEvent;
	import flash.text.TextField;

	import net.web20games.game.IDocument;
	import net.web20games.game.IContainer;

	public class Document extends Sprite implements IDocument {
		private var _container:IContainer;
	
		public function Document():void {
			_diceBtn.addEventListener(MouseEvent.CLICK, onDiceClick);
		}

		public function setContainer(container:IContainer):Object {
			_container = container;
			return {modelClass: Model};
		}

		public function enableMove(moveEnabled:Boolean):void {
		}

		// --------------------------------------------------------------------------

		public function onInit(playNicks0:Array, indexMe:int):void {
		}

		public function onMove():void {
			var model:Model = _container.model as Model;
			var data:Object = model.lastMove.data;
			if (data is Array && data[0] == 'rand') {
				var limit:int = data[1];
				var value:int = data[2];
				_dice.text = "limit: " + limit + ", value: " + value;
			}
		}
		
		public function onMoveBatch():void {
		}

		public function onLeave():void {
		}

		public function onTimeout():void {
		}

		// --------------------------------------------------------------------------
		
		private function onDiceClick(event:MouseEvent):void {
			_container.move(['rand', 6]);
		}
	}
}