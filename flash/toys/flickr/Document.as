package {
	import flash.display.*;
	import flash.net.*;
	import flash.events.*;

	import multiflame.toy.IContainer;
	import multiflame.toy.IToy;
	import multiflame.toy.Constants;

	public class Document extends Sprite implements IToy {
		private var _container:IContainer;
		private var _mode:int;

		private var _editor:Editor;
		private var _gallery:Gallery;
		private var _waiting:Ball = new Ball();
		private var _previewClicked:Boolean;

		public function setContainer(container:IContainer, mode:int, config:Array):Array {
			_container = container;
			_mode = mode;

			graphics.beginFill(0x000000);
			graphics.drawRect(0, 0, 500, 500);
			graphics.endFill();

			switch (mode) {
			case Constants.MODE_DEMO:
				config = demoConfig();
				doStart(config);
				break;
			case Constants.MODE_CONFIG:
				_waiting.x = 250;
				_waiting.y = 265;
				addEventListener(Event.ADDED, onAdded);
				addEventListener(MouseEvent.MOUSE_MOVE, onMove);

				_editor = new Editor(container);
				addChild(_editor);
				_editor.addEventListener(Editor.PREVIEW, onPreview);

				config = demoConfig();
				_editor.config = config;
				doStart(config);
				break;
			case Constants.MODE_REAL:
				try {
					doStart(config);
				} catch(e:Error) {
				}
				break;
			}

			return [500, 500];
		}

		// ---------------------------------------------------------------------------

		private function demoConfig():Array {
			var config:Array = [];
			config["userName"] = "web20games";
			config["type"]     = "tags";
			config["value"]    = "";
			return config;
		}

		private function doStart(config:Array):void {
			if (_gallery != null) {
				removeChild(_gallery);
			}

			if (_mode == Constants.MODE_CONFIG) {
				if (!contains(_waiting)) {
					addChild(_waiting);
				}
			}

			_gallery = new Gallery(config["userName"], 500, 500);
			_gallery.addEventListener(Gallery.FIND_OK, onFindOK);
			_gallery.addEventListener(Gallery.FIND_NG, onFindNG);
			if (config["type"] == "tags") {
				_gallery.findByTags(config["value"]);
			} else if (config["type"] == "set") {
				_gallery.findBySet(config["value"]);
			}
			addChild(_gallery);
		}

		public function stop():void {
			if (_gallery != null) {
				removeChild(_gallery);
				_gallery = null;
			}
		}

		// --------------------------------------------------------------------------

		private function onAdded(event:Event):void {
			if (_editor != null) {
				setChildIndex(_editor, numChildren - 1);
			}
		}
		
		private function onPreview(event:Event):void {
			_previewClicked = true;
			doStart(_editor.config);
		}
		
		private function onFindOK(event:Event):void {
			if (contains(_waiting)) {
				removeChild(_waiting);
			}
		}
		
		private function onFindNG(event:Event):void {
			if (contains(_waiting)) {
				removeChild(_waiting);
			}
		}
		
		private function onMove(event:MouseEvent):void {
			if (!_previewClicked) {
				return;
			}
			if (event.stageY < _editor.height) {
				_editor.y = 2;
			} else {
				_editor.y = -1000;
			}
		}
	}
}