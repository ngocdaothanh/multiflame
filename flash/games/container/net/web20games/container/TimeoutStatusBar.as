package net.web20games.container {
	import flash.display.*;
	import flash.events.*;
	import flash.utils.*;
	import flash.text.*;
	
	import com.adobe.webapis.gettext.GetText;
	
	public class TimeoutStatusBar extends Timer {
		public static const TIMEOUT:String = "TIMEOUT";
		
		private var _textField:TextField;

		private var _text:String;
		private var _moveSecLeft:int;
		private var _totalSecLeft:int;
		
		// timeout: [s]
		public function TimeoutStatusBar(textField:TextField):void {
			super(1000);
			_textField = textField;

			addEventListener(TimerEvent.TIMER, onTimer);
			addEventListener(TimerEvent.TIMER_COMPLETE, onTimerComplete);
		}
		
		// Autostart if timeout > 0
		// timeout, total: [s]
		public function setStatus(text:String, moveSecLeft:int = 0, totalSecLeft:int = 0):void {
			_text = text;
			_moveSecLeft = moveSecLeft;
			_totalSecLeft = totalSecLeft;

			reset();
			if (moveSecLeft == 0) {
				repeatCount = totalSecLeft;
			} else if (totalSecLeft == 0) {
				repeatCount = moveSecLeft;
			} else {
				repeatCount = (moveSecLeft < totalSecLeft)? moveSecLeft : totalSecLeft;
			}
			update();

			if (repeatCount > 0) {
				start();
			}
		}

		private function onTimer(event:TimerEvent):void {
			_moveSecLeft--;
			_totalSecLeft--;
			update();
		}
		
		private function update():void {
			_textField.text = _text;
			if (_moveSecLeft > 0) {
				_textField.appendText(" " + s2mss(_moveSecLeft));
				if (_totalSecLeft > 0) {
					_textField.appendText(" / " + s2mss(_totalSecLeft));
				}
			} else if (_totalSecLeft > 0) {
				_textField.appendText(" " + s2mss(_totalSecLeft));
			}
		}
		
		private function onTimerComplete(event:TimerEvent):void {
			_textField.text = _text + " " + _("TIMEOUT");
			var e:Event = new Event(TIMEOUT);
			dispatchEvent(e);
		}
		
		// Convert seconds to m:ss
		private function s2mss(s:int):String {
			var m:String = "" + int(s/60);
			
			var ss:String;
			var r:int = int(s%60);
			if (r < 10) {
				ss = "0" + r;
			} else {
				ss = "" + r;
			}
			
			return (m + ":" + ss);
		}
		
		// --------------------------------------------------------------------------
		
		private function _(id:String):String {
			return LoadScreen.clientGetText._(id);
		}
	}
}