package multiflame.game {
	public interface ILogin {
		function set communicator(value:ICommunicator):void;

		function onLoginComplete():void;

		function onDisconnect():void;
	}
}