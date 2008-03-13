package {
	import flash.display.*;
	import flash.events.*;

	import multiflame.utils.Captcha;

	public class Document extends Sprite {
		private var _captcha:Captcha;

		public function Document():void {
			_captcha = new Captcha('localhost', 443);
			_captcha.addEventListener(Event.COMPLETE, onComplete);
			_refreshBtn.addEventListener(MouseEvent.CLICK, refresh);
		}

		private function onComplete(event:Event):void {
			addChild(_captcha.img);
		}

		private function refresh(event:MouseEvent):void {
			_captcha.receive();
		}
	}
}