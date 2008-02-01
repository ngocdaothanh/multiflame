package net.web20games.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import flash.net.*;

	import com.adobe.webapis.gettext.GetText;
	import net.web20games.utils.*;
	import net.web20games.container.events.*;

	/*
	* Load assets:
	* * Client mofile (skipped for English)
	* * Game mofile (skipped for English)
	* * Game SWF
	* * Game information
	* * Toy config
	* * Toy SWF
	*/
	public class LoadScreen extends Sprite {
		public static var clientGetText:GetText;
		public static var gameGetText:GetText;
		
		private var _channel:Channel;
		private var _container:RoomTab;

		public function LoadScreen():void {
			clientGetText = new GetText();
			clientGetText.addEventListener(GetText.COMPLETE, onClientGetTextEvent);
			clientGetText.addEventListener(GetText.IO_ERROR, onClientGetTextEvent);
			clientGetText.addEventListener(GetText.ERROR, onClientGetTextEvent);

			gameGetText = new GetText();
			gameGetText.addEventListener(GetText.COMPLETE, onGameGetTextEvent);
			gameGetText.addEventListener(GetText.IO_ERROR, onGameGetTextEvent);
			gameGetText.addEventListener(GetText.ERROR, onGameGetTextEvent);

			_channel = Channel.instance;
			_channel.addEventListener(GameInfoEvent.LOCAL, onGameInfoLocal);
		}
		
		private function onGameInfoLocal(event:GameInfoEvent):void {
			if (event.type != GameInfoEvent.LOCAL) {
				return;
			}
			if (_channel.locale == 'en') {
				onGameGetTextEvent(null);
			} else {
				if (_channel.id < 0) {
					clientGetText.install(_channel.locale + ".mo");
				} else {
					clientGetText.install("/games/" + _channel.locale + ".mo");
				}
			}
		}
		
		private function onClientGetTextEvent(event:Event):void {
			if (_channel.id < 0) {
				gameGetText.install("mo/" + _channel.locale + ".mo");
			} else {
				gameGetText.install("/games/" + _channel.id + "/" + _channel.locale + ".mo");
			}
		}

		private function onGameGetTextEvent(event:Event):void {
			var loader:Loader = new Loader();
			loader.contentLoaderInfo.addEventListener(Event.INIT, onGameLoadInit);
			loader.contentLoaderInfo.addEventListener(IOErrorEvent.IO_ERROR, onGameLoadError);

			_statusLbl.htmlText = _("Loading game...");
			try {
				// Check if this is development mode
				if (_channel.id < 0) {
					loader.load(new URLRequest("game.swf"));
				} else {
					loader.load(new URLRequest("/gwv/" + _channel.id + "/" + _channel.gameVersion));
				}
			} catch (e:Error) {
				_statusLbl.htmlText = _("Could not load game.");
			}
		}

		private function onGameLoadInit(event:Event):void {
			_channel.addEventListener(GameInfoEvent.REMOTE, onGameInfoRemote);
			_container = RoomTab.instance;
			_container.gameDocument = event.target.content as Sprite;

			_statusLbl.htmlText = _("Loading game information...");
			_channel.getGameInfo();
		}
		
		private function onGameLoadError(event:Event):void {
			_statusLbl.htmlText = _("Could not load game.") + "<br />" +
				_("Please refresh this page after a few minutes.");
		}
		
		private function onGameInfoRemote(event:GameInfoEvent):void {
			switch (event.code) {
			case GameInfoEvent.CONNECTION_ERROR:
				_statusLbl.htmlText = StringUtil.substitute(_("{0} is down for maintenance."), Channel.WEB_HOST) +
					"<br />" +	_("Please refresh this page after a few minutes.");
				break;
			case GameInfoEvent.NO_GAME:
				_statusLbl.htmlText = _("Could not find the game.") + "<br />" +
					_("Please refresh this web page or clean the browser's cache.");
				break;
			case GameInfoEvent.OK:
				break;
			}
		}

		private function _(id:String):String {
			return clientGetText._(id);
		}
	}
}