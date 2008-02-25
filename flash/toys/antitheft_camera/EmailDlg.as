package {
	import flash.display.*;
	import flash.text.*;
	import flash.net.*;
	import flash.events.*;
	import flash.utils.ByteArray;

	import com.adobe.images.JPGEncoder;

	public class EmailDlg extends Sprite {
		public static const OK:String = "OK";

		private var _emailURL:String;

		private var _encryptedCode:ByteArray;
		private var _captchaLoader:Loader;

		public function EmailDlg(captchaURL:String, emailURL:String):void {
			_emailURL = emailURL;

			_codeInput.border = _emailInput.border = _ok.border = true;
			_ok.selectable = false;
			_ok.addEventListener(MouseEvent.CLICK, onOKClick);

			var ld:URLLoader = new URLLoader();
			ld.addEventListener(IOErrorEvent.IO_ERROR, onError);
			ld.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onError);
			ld.addEventListener(Event.COMPLETE, onCaptchaLoaded);
			ld.dataFormat = URLLoaderDataFormat.BINARY;
			//ld.load(new URLRequest(captchaURL));
			ld.load(new URLRequest("http://localhost:3000" + captchaURL));
		}

		public function email(subject:String, body:String, img:BitmapData):void {
			var enc:JPGEncoder = new JPGEncoder(60);
			var o:Object = {
				encryptedCode: _encryptedCode,
				code:          _codeInput.text,
				email:         _emailInput.text,
				subject:       subject,
				body:          body,
				jpg:           enc.encode(img)
			};
			var ba:ByteArray = new ByteArray();
			ba.writeObject(o);
			ba.position = 0;

			//var request:URLRequest = new URLRequest(_emailURL);
			var req:URLRequest = new URLRequest("http://localhost:3000" + _emailURL);
			req.contentType = "application/octet-stream";
			req.method = URLRequestMethod.POST;
			req.data = ba;
			sendToURL(req);
		}

		// ---------------------------------------------------------------------------

		private function onError(event:Event):void {
			_descLbl.text = "Could not connect to server.";
		}

		private function onCaptchaLoaded(event:Event):void {
			var ba:ByteArray = event.target.data as ByteArray;		
			var a:Array = ba.readObject();

			_encryptedCode = a[0];

			_captchaLoader = new Loader();
			_captchaLoader.contentLoaderInfo.addEventListener(Event.COMPLETE, onCaptchaDisplayed);
			_captchaLoader.loadBytes(a[1]);
			_captchaLoader.x = _captchaLoader.y = 5;
			addChild(_captchaLoader);
		}

		private function onCaptchaDisplayed(event:Event):void {
			_captchaLoader.x = (width - _captchaLoader.width)/2;
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