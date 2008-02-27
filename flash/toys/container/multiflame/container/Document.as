package multiflame.container {
	import flash.display.*;
	import flash.text.*;
	import flash.events.*;
	import flash.net.*;

	import com.adobe.webapis.gettext.GetText;

	import multiflame.toy.IContainer;
	import multiflame.toy.IToy;
	import multiflame.toy.Constants;

	public class Document extends Sprite implements IContainer {
		private var _mode:int;

		private var _gt:GetText;

		private var _id:int;
		private var _locale:String;
		private var _params:String;

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

		// ---------------------------------------------------------------------------

		private function determineMode():void {
			// When loaded from the web server, the URL is in the form:
			// .../id/locale/params
			var url:String = loaderInfo.loaderURL;
			if (url.indexOf("file://") == 0) {
				_id     = -1;
				_locale = null;
				_params = null;
			} else {
				var a:Array = url.split('/');
				var len:int = a.length;
				_id     = int(a[len - 3]);
				_locale = a[len - 2];
				_params = a[len - 1];
				if (_params == "") {
					_params = null;
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
			var d:Array = toy.setContainer(this, _mode, parse(_params));

			if (_mode == Constants.MODE_CONFIG) {
				stage.scaleMode = StageScaleMode.NO_SCALE;
				sprite.x = (500 - d[0])/2;
				sprite.y = (500 - d[1])/2;
			} else {
				if (d[0] < d[1]) {
					sprite.height = 500;
					sprite.scaleX = sprite.scaleY;
				} else {
					sprite.width = 500;
					sprite.scaleY = sprite.scaleX;
				}
			}
		}

		private function parse(params:String):Array {
			return null;
		}
	}
}
