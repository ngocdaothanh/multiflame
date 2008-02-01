package {
	import flash.display.*;
	import flash.net.*;
	import flash.events.*;

	public class Document extends Sprite {
		public static const MODE_DEMO:int = 0;
		public static const MODE_EDIT:int = 1;
		public static const MODE_TOY:int  = 2;

		private var _mode:int;

		private var _gallery:Gallery;
		private var _waiting:Ball = new Ball();
		private var _previewClicked:Boolean;

		public function Document():void {
			var b:Sprite = new Sprite();
			b.graphics.beginFill(0x000000);
			b.graphics.drawRect(0, 0, 500, 530);
			b.graphics.endFill();
			addChild(b);

			var flashVars:Object = this.root.loaderInfo.parameters;
			if (flashVars != null) {
				if (flashVars["mode"] == "demo") {
					_mode = MODE_DEMO;
				} else if (flashVars["mode"] == "edit") {
					_mode = MODE_EDIT;
				} else {
					_mode = MODE_TOY;
				}
			} else {
				_mode = MODE_TOY;
			}

			var config:Config;
			switch (_mode) {
			case MODE_DEMO:
				removeChild(_editor);
				config = new Config();
				config.demo();
				doStart(config);
				break;
			case MODE_EDIT:
				_waiting.x = 250;
				_waiting.y = 265;
				addEventListener(Event.ADDED, onAdded);
				addEventListener(MouseEvent.MOUSE_MOVE, onMove);
				_editor.addEventListener(Editor.PREVIEW, onPreview);

				config = new Config();
				config.demo();
				_editor.config = config;
				doStart(config);
				break;
			case MODE_TOY:
				removeChild(_editor);
				try {
					var url:String = loaderInfo.loaderURL;
					var a:Array = url.split('/');
					start(a[a.length - 1]);
				} catch(e:Error) {
					config = new Config();
					config.demo();
					doStart(config)
				}
				break;
			}
		}

		public function start(config:String):void {
			var c:Config = new Config();
			c.parse(config);
			doStart(c);
		}

		private function doStart(config:Config):void {
			if (_gallery != null) {
				removeChild(_gallery);
			}

			if (_mode == MODE_EDIT) {
				if (!contains(_waiting)) {
					addChild(_waiting);
				}
			}

			_gallery = new Gallery(config.userName, 500, 500);
			_gallery.addEventListener(Gallery.FIND_OK, onFindOK);
			_gallery.addEventListener(Gallery.FIND_NG, onFindNG);
			if (config.type == "tags") {
				_gallery.findByTags(config.value);
			} else if (config.type == "set") {
				_gallery.findBySet(config.value);
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
			setChildIndex(_editor, numChildren - 1);
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