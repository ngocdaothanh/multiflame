package multiflame.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import fl.controls.*;
	import flash.net.*;
	import flash.utils.ByteArray;

	import gs.TweenFilterLite;

	import multiflame.utils.*;
	import multiflame.container.events.*;
	import multiflame.game.Constants;

	public class LoginTab extends MovieClip {
		private var _channel:Channel;
		private var _captcha:Captcha;
		
		public function LoginTab():void {
			_channel = Channel.instance;

			_captcha = new Captcha(_channel.host, _channel.port);
			_captcha.addEventListener(Event.COMPLETE, onCaptcha);

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

			if (_captcha.img == null) {
				_captcha.receive();
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

				_channel.login(_codeInput.text, _captcha.encryptedCode, _nickInput.text);
			}
		}

		//---------------------------------------------------------------------------

		private function onCaptcha(event:Event):void {
			if (_captcha.img != null) {
				removeChild(img);
			}
			var img:DisplayObject = _captcha.img;
			img.x = 309;
			img.y = 218;
			addChild(img);
		}

		//---------------------------------------------------------------------------

		private function onLoginMe(event:LoginoutEvent):void {
			switch (event.code) {
			case LoginoutEvent.CONNECTION_ERROR:
				_statusLbl.htmlText = StringUtil.substitute(_("We are upgrading {0}."), Config.WEB_SITE) + "<br />" +	_("Please refresh this page after a few minutes.");
				break;
			case LoginoutEvent.WRONG_CAPTCHA:
				_statusLbl.htmlText = _("Wrong code");
				_captcha.receive();
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
				_statusLbl.htmlText = _("This flash file is old.") + "<br />" + _("Please refresh this page or clean the browser's cache.");
				break;
			case LoginoutEvent.NO_GAME:
				_statusLbl.htmlText = _("Could not find the game.") + "<br />" + _("Please refresh this web page or clean the browser's cache.");
				break;
			case LoginoutEvent.OLD_GAME_VERSION:
				_statusLbl.htmlText = _("The game file is old.") + "<br />" + _("Please refresh this web page or clean the browser's cache.");
				break;
			case LoginoutEvent.WRONG_NICK_OR_PASSWORD:
				_statusLbl.htmlText = _("Wrong nick or password");
				break;
			case LoginoutEvent.NOT_FRIENDS:
				_statusLbl.htmlText = _("This channel is private.") + "<br />" + StringUtil.substitute(_("You must be a friend of {0} in order to login."), _channel.channel);
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