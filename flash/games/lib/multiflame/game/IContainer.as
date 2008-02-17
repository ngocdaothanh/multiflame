package multiflame.game {
	public interface IContainer {
		/**
		 * Translates the given text into the current language on the container.
		 */
		function _(id:String):String;

		/**
		 * @return
		 * TweenLite class. See http://www.TweenLite.com.
		 */
		function get TweenLite():*;

		/**
		 * @return
		 * TweenFilerLite class. See http://www.TweenLite.com.
		 */
		function get TweenFilterLite():*;

		// ---------------------------------------------------------------------------

		/**
		 * Only called by the game.
		 */

		/**
		 * @return
		 * {
		 * 	nPlayers,
		 * 	moveSec,
		 * 	totalMin
		 * }
		 */
		function get baseConfig():Object;

		/**
		 * If you want to use extended config, please define a custom config dialog.
		 */
		function get extendedConfig():Object;

		/**
		 * @return
		 * Nicks of players who initially joined the game. Players can resign during the game.
		 */
		function get nicks0():Array;

		/**
		 * @return
		 * Index of this player, negative if this player did not joined the game.
		 */
		function get indexMe();

		/**
		 * If move time is nearly over and enqueueMove() has not been called, the game
		 * container will automatically call enqueueMove(defaultMove) if defaultMove
		 * is not null.
		 */
		function set defaultMove(value:Object):void;

		function get defaultMove():Object;

		/**
		 * @return
		 * Array of results (NONE/LOST/DREW/WON) for each player.
		 */
		function get gameResult():Array;

		/**
		 * Extra information to display when the game is over.
		 */
		function set extraGameResult(value:String):void;

		/**
		 * "Action" means onMove()/onResign()/onTimeout(). The game calls this method
		 * to notify that game action processing has finished. The processing may take
		 * a lot of time (up to some seconds) when the game uses physics engine.
		 *
		 * @param value
		 * OVER                The game is over.
		 * ANY                 Any player can make the next move.
		 * Non-negative number Index of the player who should make the next move.
		 */
		function onActionDone(result:int):void;

		/**
		 * For immediate game, it is the index of the player who should make the next move.
		 */
		function get lastActionResult():int;

		/**
		 * Enqueue a move to the server.
		 */
		function enqueueMove(data:Object):void;

		// ---------------------------------------------------------------------------

		/**
		 * Only called by the config dialog.
		 */

		function get baseConfigRange():Object;
		function config(baseConfig:Object, extendedConfig:Object):void;
		function join():void;
		function unjoin():void;
	}
}