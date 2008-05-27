package {
	import flash.display.*;
	import flash.text.*;
	import flash.net.*;
	import flash.events.*;
	import flash.utils.ByteArray;

	import multiflame.toy.IContainer;
	import multiflame.toy.Constants;

	import multiflame.utils.Captcha;
	import multiflame.utils.JpgMail;
	import multiflame.utils.Config;

	public class EmailDlg extends Sprite {
		public static const OK:String = "OK";

		private var _container:IContainer;

		private var _captcha:Captcha;
		private var _encryptedCode:ByteArray;

		private var _jpgMail:JpgMail;

		public function EmailDlg(container:IContainer):void {
			_container = container;
			_codeLbl.text   = _("Code");
			_emailLbl.text  = _("Email");
			_embedLbl.text  = _("Embed");
			_statusLbl.text = _("Images will be sent to your email when there is movement.");

			_codeInput.border = _emailInput.border = _embedInput.border = _ok.border = true;
			_embedInput.text = container.embed(null);
			_ok.selectable = false;
			_ok.addEventListener(MouseEvent.CLICK, onOKClick);

			_captcha = new Captcha(Config.DEFAULT_GSERVER_HOST, Config.DEFAULT_GSERVER_PORT);
			_captcha.addEventListener(Event.COMPLETE, onCaptchaLoaded);
			_captcha.receive();

			_jpgMail = new JpgMail(Config.DEFAULT_GSERVER_HOST, Config.DEFAULT_GSERVER_PORT);
		}

		public function mail(subject:String, body:String, img:BitmapData):void {
			_jpgMail.mail(_captcha.encryptedCode, _codeInput.text,
				_emailInput.text, subject, body, img);
		}

		// ---------------------------------------------------------------------------

		private function _(id:String):String {
			return _container._(id);
		}

		private function onError(event:Event):void {
			_statusLbl.text = _("Could not connect to server.");
		}

		private function onCaptchaLoaded(event:Event):void {
			var img:DisplayObject = _captcha.img;
			img.x = img.y = 5;
			addChild(img);
		}

		private function onOKClick(event:Event):void {
			var r:RegExp = /^[a-z0-9][-._a-z0-9]*@(([a-z0-9][-_a-z0-9]*\.)+[a-z]{2,6}|((25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(25[0-5]|2[0-4]\d|[01]?\d\d?))$/
			if (!r.test(_emailInput.text)) {
				_emailInput.text = "";
			} else {
				dispatchEvent(new Event(OK));
			}
		}
	}
}
