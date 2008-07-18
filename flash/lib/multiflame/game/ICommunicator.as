package multiflame.game {
	public interface IContainer {
		function onLoad(id:int, channel:String, locale:String,
			containerVersion:int, gameVersion:int, host:String, port:int):void;

		function requestGameInfo():void;

		function reqLogin(code:String, encryptedCode:ByteArray, nick:String):void;
	}
}