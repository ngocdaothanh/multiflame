package multiflame.container {
	import flash.display.*;
	import flash.text.*;
	import flash.events.*;
	import flash.net.*;

	import com.adobe.webapis.gettext.GetText;

	import multiflame.toy.IContainer;
	import multiflame.toy.IToy;
	import multiflame.toy.Constants;

	import multiflame.utils.DataInURL;
	import multiflame.utils.Config;

	public class Document extends Sprite implements IContainer {
		private var _mode:int;

		private var _gt:GetText;

		private var _id:int;
		private var _locale:String;
		private var _config:Array;
		private var _dimensions:Array;

		public function Document():void {			
			determineMode();

			if (_id > 0 && _locale != "en") {
				_gt = new GetText();
				_gt.addEventListener(GetText.COMPLETE, loadToy);
				_gt.addEventListener(GetText.IO_ERROR, loadToy);
				_gt.addEventListener(GetText.ERROR, loadToy);
				_gt.install("/toys/" + _id + "/" + _locale + ".mo");
			} else {
				loadToy(null);
			}
		}

		public function _(id:String):String {
			if (_gt == null) {
				return id;
			} else {
				return _gt._(id);
			}
		}

		public function embed(config:Array):String {
			var url:String = "http://" + Config.WEB_SITE + "/toys/" + _id + "/" + _locale;
			var string:String = configToString(config);
			if (string != null) {
				url += "/" + string;
			}
			return '<object width="' + _dimensions[0] + '" height="' + _dimensions[1] +
				'"><param name="movie" value="' + url + '"></param>' +
				'<param name="wmode" value="transparent"></param>' +
				'<embed src="' + url + '" type="application/x-shockwave-flash" wmode="transparent" ' +
				'width="' + _dimensions[0] + '" height="' + _dimensions[1] + '"></embed></object>';
		}

		// ---------------------------------------------------------------------------

		private function determineMode():void {
			// When loaded from the web server, the URL is in the form:
			// http://host/toys/id/locale or http://host/toys/id/locale/config			
			var url:String = loaderInfo.loaderURL;
			if (url.indexOf("file://") == 0) {
				_id     = -1;
				_locale = null;
				_config = null;
			} else {
				var a:Array = url.split('/');
				_id     = int(a[4]);
				_locale = a[5];
				if (a.length == 6) {					
					_config = null;
				} else {
					_config = stringToConfig(a[6]);
				}
			}

			var flashVars:Object = root.loaderInfo.parameters;
			if (flashVars != null) {
				if (flashVars["mode"] == "demo") {
					_mode = Constants.MODE_DEMO;
				} else if (flashVars["mode"] == "edit") {
					_mode = Constants.MODE_CONFIG;
				} else {
					_mode = Constants.MODE_REAL;
				}
			} else {
				_mode = Constants.MODE_REAL;
			}
		}

		private function loadToy(event:Event):void {
			var loader:Loader = new Loader();
			loader.contentLoaderInfo.addEventListener(IOErrorEvent.IO_ERROR, onToyLoadError);
			loader.contentLoaderInfo.addEventListener(Event.INIT, onToyLoadInit);			

			_statusLbl.text = "Loading toy...";
			if (_id > 0) {
				loader.load(new URLRequest("/toys/" + _id + "/toy.swf"));
			} else {
				loader.load(new URLRequest("toy.swf"));
			}
		}

		private function onToyLoadError(event:Event):void {
			_statusLbl.text = "Could not load toy.";
		}
		
		private function onToyLoadInit(event:Event):void {
			removeChild(_statusLbl);
			var toy:IToy = event.target.content as IToy;
			var sprite:Sprite = event.target.content as Sprite;
			addChild(sprite);
			_dimensions = toy.setContainer(this, _mode, _config);

			if (_mode == Constants.MODE_CONFIG) {
				stage.scaleMode = StageScaleMode.NO_SCALE;
				sprite.x = (500 - _dimensions[0])/2;
				sprite.y = (500 - _dimensions[1])/2;
			} else {
				if (_dimensions[0] < _dimensions[1]) {
					sprite.height = 500;
					sprite.scaleX = sprite.scaleY;
				} else {
					sprite.width = 500;
					sprite.scaleY = sprite.scaleX;
				}
			}
		}

		private function stringToConfig(string:String):Array {
			try {
				var v:URLVariables = new URLVariables(DataInURL.decode(string));
				var config:Array = [];
				for (var key:String in v) {
					config[key] = v[key];
				}
				return config;
			} catch (e:Error) {
			}
			return null;
		}

		private function configToString(config:Array):String {
			try {
				var v:URLVariables = new URLVariables();
				for (var key:String in config) {
					v[key] = config[key];
				}
				return DataInURL.encode(v.toString());
			} catch (e:Error) {
			}
			return null;
		}
	}
}
