/*
*	This is template used for creating a new game.
*	TODO: Follow intructions (numbered-comment).
*/

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

		// [1] Other variables come here.
		
		public function Document():void {
			//player1Nick.htmlText = "";
			//player2Nick.htmlText = "";
			player1Nick.text = "";
			player2Nick.text = "";
			
			// [2] Create a movie named "board" and uncomment next line.
			//board.addEventListener(MouseEvent.CLICK, onClick);
		}
		
		// DO NOT modify this function.
		public function setContainer(container:IContainer):Object {
			_container = container;
			return {modelClass: Model, introSprite: new IntroSprite(container)};
		}
		
		// DO NOT modify this function.
		public function enableMove(moveEnabled:Boolean):void {
			_moveEnabled = moveEnabled;
		}

		// --------------------------------------------------------------------------

		public function onInit(playNicks0:Array, indexMe:int):void {
			_playNicks0 = playNicks0;
			_indexMe = indexMe;
			// [3] Other needed variables come here.
			
			// [4] Initialize game-variables.
			
			// Display nicks.
			//player1Nick.htmlText = _container.playerLink(playNicks0[0]);
			//player2Nick.htmlText = _container.playerLink(playNicks0[1]);
			player1Nick.text = playNicks0[0];
			player2Nick.text = playNicks0[1];
			
			// [5] Draw initial board.
			
		}

		public function onMove():void {
			// [6] Draw pieces on each move done by player.
		}
		
		public function onMoveBatch():void {
		}

		public function onLeave():void {
		}

		public function onTimeout():void {
		}

		// --------------------------------------------------------------------------
		
		private function onClick(event:MouseEvent):void {
			if (!_moveEnabled) {
				return;
			}
			
			// [7] Calculate needed value to pass to _container.move(i);
			// [8] Remember to call _container.move(i);

		}

		// [9] Other private functions come here.
		
	}
}
