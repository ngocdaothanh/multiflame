package {
	import flash.display.Sprite;
	import flash.text.*;
	import flash.events.*;
	import fl.controls.*;

	import multiflame.toy.IContainer;

	public class Editor extends Sprite {
		public static const PREVIEW:String = "EDITOR_PREVIEW";

		private var _container:IContainer;

		public function Editor(container:IContainer):void {
			_container = container;
			addEventListener(Event.ADDED, onAdded);
			_previewBtn.addEventListener(MouseEvent.CLICK, onPreview);
		}

		public function set config(config:Array):void {
			_userName.text      = config["userName"];
			_tagsRadio.selected = config["type"] == "tags";
			_setRadio.selected  = !_tagsRadio.selected;
			_value.text         = config["value"];
		}

		public function get config():Array {
			var ret:Array = [];
			ret["userName"] = _userName.text;
			ret["type"]     = _tagsRadio.selected ? "tags" : "set";
			ret["value"]    = _value.text;
			return ret;
		}

		// ---------------------------------------------------------------------------

		private function onAdded(event:Event):void {
			_userNameLbl.text = _("User name");
			_tagsRadio.label  = _("Tags");
			_setRadio.label   = _("Set");
			_embedLbl.text    = _("Embed");
			_previewBtn.label = _("Preview");
		}

		private function _(id:String):String {
			return _container._(id);
		}

		private function onPreview(event:MouseEvent):void {
			_embed.text = _container.embed(config);
			var e:Event = new Event(PREVIEW);
			dispatchEvent(e);
		}
	}
}