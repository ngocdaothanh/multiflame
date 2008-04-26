package multiflame.game {
	/**
	 * Config dialog must be a Sprite or its subclass.
	 */
	public interface IConfigDlg {
		function setContainer(container:IContainer):void;

		/**
		 * If the player logs in at the the middle of NEW state, onInit and
		 * a series of onJoin will be called by the game container.
		 *
		 * @param nick
		 * Nick of the player who is inviting.
		 *
		 * @param me
		 * true if this player is me.
		 */
		function onConfig(nick:String, me:Boolean, baseConfig:Object, extendedConfig:Object):void;

		function onJoin(nick:String):void;
		function onUnjoin(nick:String):void;
		function onTimeout():void;

		/**
		 * When a game is over, the config dialog is redisplayed. This method should
		 * display reresult of the last game.
		 */
		function onGameResult(nicks0:Array, result:Array, extra:String):void;
	}
}