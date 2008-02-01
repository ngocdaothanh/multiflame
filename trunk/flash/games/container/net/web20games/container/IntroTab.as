package net.web20games.container {
	import flash.display.*;
	import flash.events.*;

	public class IntroTab extends Sprite {
		public function IntroTab():void {
			addEventListener(Event.ADDED, onAdded);
		}

		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}

			var s:Sprite = RoomTab.instance.introSprite;
			if (s != null && !contains(s)) {
				addChild(s);
			}
		}		
	}
}