package multiflame.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import flash.net.URLVariables;

	import multiflame.utils.DataInURL;
	import multiflame.container.events.*;

	public class Document extends flash.display.Sprite {
		private var _channel:Channel;

		private var _loadScreen:LoadScreen;
		private var _gameScreen:GameScreen;

		public function Document():void {
			_channel = Channel.instance;
			_channel.addEventListener(GameInfoEvent.REMOTE, onGameInfoRemote);

			_gameScreen = new GameScreen();

			_loadScreen = new LoadScreen();
			addChild(_loadScreen);

			// In production mode, the URL from which the client SWF file is downloaded
			// is in the form:
			// http://host[:port]/game_container_with_versions/:id/:channel/:locale/:container_version/:game_version
			//
			// clientVersion and version are automatically extracted by FIFO server
			// from connect parameters (swfUrl)
			//
			// In development mode, the SWF file is named <locale>.swf and put in the
			// game folder
			var url:String = loaderInfo.loaderURL;

			var id:int;
			var channel:String;
			var locale:String;
			var containerVersion:int;
			var gameVersion:int;
			var host:String;
			var port:int;

			// Check if this is development mode
			var a:Array = url.split('/');
			if (url.indexOf("file://") == 0) {
				id               = -1;  // Mark that this is developement mode
				channel          = "localhost";
				locale           = a[a.length - 1].split(".")[0];
				containerVersion = 1;
				gameVersion      = 1;
				host             = "localhost";
				port             = 443;
			} else {
				id               = a[4];
				channel          = DataInURL.decode(a[5]);
				locale           = a[6];
				containerVersion = int(a[7]);
				gameVersion      = int(a[8]);
				host             = DataInURL.decode(a[9]);
				port             = int(DataInURL.decode(a[10]));
			}
			_channel.broadcastGameInfoLocal(id, channel, locale, containerVersion, gameVersion, host, port);
		}

		private function onGameInfoRemote(event:GameInfoEvent) {
			if (event.code == GameInfoEvent.OK) {
				removeChild(_loadScreen);
				addChild(_gameScreen);
			}
		}
	}
}