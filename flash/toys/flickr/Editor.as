package {
	import flash.display.Sprite;
	import flash.text.*;
	import flash.events.*;
	import fl.controls.*;

	public class Editor extends Sprite {
		public static const PREVIEW:String = "EDITOR_PREVIEW";
		private static const SWF_WIDTH:int = 500;
		private static const SWF_HEIGHT:int = 560;

		public function Editor():void {
			_previewBtn.addEventListener(MouseEvent.CLICK, onPreview);
		}
		
		public function set config(config:Config):void {
			_userName.text      = config.userName;
			_tagsRadio.selected = config.type == "tags";
			_setRadio.selected  = !_tagsRadio.selected;
			_value.text         = config.value;
		}

		public function get config():Config {
			var ret:Config = new Config();
			ret.userName = _userName.text;
			ret.type     = _tagsRadio.selected ? "tags" : "set";
			ret.value    = _value.text;
			return ret;
		}

		private function onPreview(event:MouseEvent):void {
			var url = "http://web20games.net/twc/1/" + config.toEncodedString();
			_embed.text = '<object width="' + SWF_WIDTH + '" height="' + SWF_HEIGHT +
				'"><param name="movie" value="' + url + '"></param>' +
				'<param name="wmode" value="transparent"></param>' +
				'<embed src="' + url + '" type="application/x-shockwave-flash" wmode="transparent" ' +
				'width="' + SWF_WIDTH + '" height="' + SWF_HEIGHT + '"></embed></object>'
			
			var e:Event = new Event(PREVIEW);
			dispatchEvent(e);
		}
	}
}