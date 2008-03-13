package {
	import flash.display.*;
	import flash.events.*;

	import multiflame.utils.Captcha;
	import multiflame.utils.JpgMail;

	public class Document extends Sprite {
		private var _captcha:Captcha;
		private var _jpgMail:JpgMail;

		public function Document():void {
			_captcha = new Captcha('localhost', 443);
			_captcha.addEventListener(Event.COMPLETE, onCaptchaComplete);

			_jpgMail = new JpgMail('localhost', 443);
			_jpgMail.addEventListener(Event.COMPLETE, onJpgMailComplete);

			_refreshBtn.addEventListener(MouseEvent.CLICK, refresh);
		}

		private function onCaptchaComplete(event:Event):void {
			trace("onCaptchaComplete");
			addChild(_captcha.img);
		}

		private function onJpgMailComplete(event:Event):void {
			trace("onJpgMailComplete");
		}

		private function refresh(event:MouseEvent):void {
			_captcha.receive();
			_jpgMail.mail(_captcha.encryptedCode, "0", "ngocdaothanh@gmail.com", "1", "2", new BitmapData(5, 5));
		}
	}
}