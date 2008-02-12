package net.web20games.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import fl.controls.*;
	import flash.net.*;
	import flash.utils.ByteArray;

	import gs.TweenFilterLite;

	import net.web20games.utils.*;
	import net.web20games.container.events.*;

	public class LoginTab extends MovieClip {
		private var _channel:Channel;
		private var _captchaTransporter:CaptchaTransporter;
		private var _captchaImg:DisplayObject;
		private var _encryptedCode:String;
		
		public function LoginTab():void {
			_channel = Channel.instance;

			_captchaTransporter = new CaptchaTransporter();
			_captchaTransporter.addEventListener(CaptchaTransporter.CAPTCHA, onCaptcha);

			addEventListener(Event.ADDED, onAdded);
		
			_codeInput.border = true;
			_codeInput.addEventListener(KeyboardEvent.KEY_UP, onInputKeyUp);
			
			_nickInput.border = true;
			_nickInput.addEventListener(KeyboardEvent.KEY_UP, onInputKeyUp);
			
			_loginBtn.addEventListener(MouseEvent.CLICK, onLoginClick);
			_loginBtn.enabled = false;

			_channel.addEventListener(LoginoutEvent.LOGIN_ME, onLoginMe);
		}

		//---------------------------------------------------------------------------

		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}

			var game:Sprite = RoomTab.instance.game;
			if (!contains(game)) {
				addChild(game);
				setChildIndex(game, 0);
				TweenFilterLite.to(game, 1.5, {type: "Color", colorize: 0x000000, amount: 0.5});
			}

			_codeLbl.text   = _("Code");
			_nickLbl.text   = _("Nick");
			_loginBtn.label = _("Login");

			_gameNameLbl.text = _channel.gameRemoteInfo["name"];
			_statusLbl.htmlText = "";			

			if (_captchaImg == null) {
				_captchaTransporter.getCaptcha();
			}
		}

		private function onInputKeyUp(event:KeyboardEvent):void {
			_codeInput.text = _codeInput.text.toUpperCase();
			if (event.charCode == 13) {
				onLoginClick(null);
			} 
		}
		
		private function onLoginClick(event:MouseEvent):void {
			if (_nickInput.type == TextFieldType.DYNAMIC) {
				return;
			}

			_codeInput.text = StringUtil.trim(_codeInput.text);
			_nickInput.text = StringUtil.trim(_nickInput.text);

			if (_codeInput.text.length != 3) {
				_statusLbl.htmlText = _("Wrong code");
			} else if (_nickInput.text == "") {
				_statusLbl.htmlText = _("Please input your nick");
			} else {
				_statusLbl.htmlText = _("Logging in...");
				_nickInput.type     = TextFieldType.DYNAMIC;
				_loginBtn.enabled   = false;

				_channel.login(_codeInput.text, _encryptedCode, _nickInput.text);
			}
		}

		//---------------------------------------------------------------------------

		private function onCaptcha(event:Event):void {
			_encryptedCode = _captchaTransporter.encryptedCode;
			if (_captchaImg != null) {
				removeChild(_captchaImg);
			}
			_captchaImg = _captchaTransporter.img;
			_captchaImg.x = 309;
			_captchaImg.y = 218;
			addChild(_captchaImg);
		}

		//---------------------------------------------------------------------------

		private function onLoginMe(event:LoginoutEvent):void {
			switch (event.code) {
			case LoginoutEvent.CONNECTION_ERROR:
				_statusLbl.htmlText = StringUtil.substitute(_("{0} is down for maintenance.<br />Please refresh this page after a few minutes."), Channel.WEB_HOST);
				break;
			case LoginoutEvent.WRONG_CAPTCHA:
				_statusLbl.htmlText = _("Wrong code");
				_captchaTransporter.getCaptcha();
				_codeInput.htmlText = "";
				break;
			case LoginoutEvent.DIFFERENT_CONTAINER_VERSION:
				_statusLbl.htmlText = _("People in this channel are using an older version<br />of this flash file, you cannot play with them.");
				break;
			case LoginoutEvent.DIFFERENT_GAME_VERSION:
				_statusLbl.htmlText = _("People in this channel are playing an older version<br />of the game, you cannot play with them.");
				break;
			case LoginoutEvent.DUPLICATE_NICK:
				_statusLbl.htmlText = _("There's already a person with the same nick in this channel.");
				break;
			case LoginoutEvent.OLD_CONTAINER_VERSION:
				_statusLbl.htmlText = _("This flash file is old.<br />Please refresh this page or clean the browser's cache.");
				break;
			case LoginoutEvent.NO_GAME:
				_statusLbl.htmlText = _("Could not find the game.<br />Please refresh this web page or clean the browser's cache.");
				break;
			case LoginoutEvent.OLD_GAME_VERSION:
				_statusLbl.htmlText = _("The game file is old.<br />Please refresh this web page or clean the browser's cache.");
				break;
			case LoginoutEvent.WRONG_NICK_OR_PASSWORD:
				_statusLbl.htmlText = _("Wrong nick or password");
				break;
			case LoginoutEvent.NOT_FRIENDS:
				_statusLbl.htmlText = StringUtil.substitute(_("This channel is private.<br />You must be a friend of {0} in order to login."), _channel.channel);
				break;
			}

			_nickInput.type   = TextFieldType.INPUT;
			_loginBtn.enabled = true;
		}
		
		//---------------------------------------------------------------------------
		
		private function _(id:String):String {
			return LoadScreen.clientGetText._(id);
		}
	}
}