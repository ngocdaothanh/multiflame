package {
	import flash.net.*;

	public class Config {
		public var userName:String;
		public var type:String;
		public var value:String;

		public function demo():void {
			userName = "web20games";
			type = "tags";
			value = "";
		}

		public function toEncodedString():String {
			var u:URLVariables = new URLVariables();
			u.userName = userName;
			u.type = type;
			u.value = value;
			return u.toString();
		}

		public function parse(s:String):void {
			var u:URLVariables = new URLVariables(s);
			userName = u.userName;
			type = u.type;
			value = u.value;
		}
	}
}