package net.web20games.game {
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
		 * The enabled property is used to mark that player inputs are being enabled/disabled.
		 * For example, on mouse events, your game must check if enabled is true before further
		 * processing.
		 */
		function get enabled():Boolean;

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

		/**
		 * defaultMove is reset to null after returned.
		 */
		function get defaultMove():Object;

		/**
		 * @return
		 * Array of results (NONE/LOST/DREW/WON) for each player.
		 */
		function get gameResult():Array;

		/**
		 * Notify to the container about the result of the last action. Sometimes the
		 * action processing code (onMove(), onResign(), onTimeout()) may take a lot
		 * of time (up to some seconds). In this case, this method should be called
		 * when the processing finishes.
		 *
		 * @param result
		 * OVER                The game is over.
		 * ANY                 Any player can make the next move.
		 * Non-negative number Index of the player who should make the next move.
		 *
		 * @param extra
		 * Extra informaiton to display when the game is over.
		 */
		function setActionResult(result:int, extra:String = null):void;

		/**
		 * "Action" means move/resign/timeout. Convenient for immediate-game, because
		 * it is the index of the player who should make the next move.
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

		function get definition():Object;
		function init(baseConfig:Object, extendedConfig:Object):void;
		function join():void;
		function unjoin():void;
	}
}