package {
	import flash.events.*;
	import flash.net.*;
	import flash.display.*;
	import flash.system.LoaderContext;

	public class BitmapURL extends EventDispatcher {
		public static const DOWNLOAD_BUFFER_DONE:String = "IMAGE_URL_DOWNLOAD_BUFFER_DONE";
		public static const DOWNLOAD_FIRST_DONE:String = "IMAGE_URL_DOWNLOAD_FIRST_DONE";
		public static const DOWNLOAD_LAST_DONE:String = "IMAGE_URL_DOWNLOAD_+AST_DONE";

		private var _farmId:String;
		private var _serverId:String;
		private var _id:String;
		private var _secret:String;
		
		private var _type:String;
		private var _bitmap:Bitmap;
		
		public function BitmapURL(farmId:String, serverId:String, id:String, secret:String):void {
			_farmId = farmId;
			_serverId = serverId;
			_id = id;
			_secret = secret;
		}
		
		public function get bitmap():Bitmap {
			var ret:Bitmap = _bitmap;
			_bitmap = null;
			return ret;
		}

		public function download(type:String, small:Boolean = false):void {
			_type = type;
			_bitmap = null;

			var loader:Loader = new Loader();
			loader.contentLoaderInfo.addEventListener(Event.COMPLETE, onDownloadOK);
			loader.contentLoaderInfo.addEventListener(IOErrorEvent.IO_ERROR, onDownloadNG);

			var context:LoaderContext = new LoaderContext();
			context.checkPolicyFile = true;
			loader.load(new URLRequest(url(small)), context);
		}
		
		// --------------------------------------------------------------------------

		private function url(small:Boolean = true):String {
			var ret:String = "http://farm" + _farmId + ".static.flickr.com/" + _serverId +
				"/" + _id + "_" + _secret;
			if (small) {
				ret += "_s";
			}
			ret += ".jpg";
			return ret;			
		}
		
		private function onDownloadOK(event:Event):void {
			_bitmap = Bitmap(event.target.content);
			_bitmap.smoothing = true;
			dispatchEvent(new Event(_type));
		}
		
		private function onDownloadNG(event:Event):void {
			dispatchEvent(new Event(_type));
		}
	}
}