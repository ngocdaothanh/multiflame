package net.web20games.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import fl.controls.*;
	
	public class TabButton extends MovieClip {
		private var _off:Boolean;
		
		public function TabButton():void {
			_off = false;
			_lbl.selectable = false;
			gotoAndStop(1);
		}
		
		public function set label(value:String):void {
			_lbl.text = value;
		}
		
		public function set off(value:Boolean):void {
			_off = value;
			if (value) {
				gotoAndStop(2);
			} else {
				gotoAndStop(1);
			}
		}
		
		public function get off():Boolean {
			return _off;
		}
	}
}
