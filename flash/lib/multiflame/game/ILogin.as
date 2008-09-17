package multiflame.game {
	public interface ILogin {
		//function set communicator(value:ICommunicator):void;
		function set communicator(value:IContainer):void;

		function onLoginComplete():void;

		function onDisconnect():void;
	}
}