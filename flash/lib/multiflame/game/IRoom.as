package multiflame.game {
	public interface IRoom {
		// ---------------------------------------------------------------------
		// Called by the communicator.

		function onNewConfig():void;
		function onNewJoin():void;
		function onNewUnjoin():void;
		function onNewTimeout():void;
		function onPlayMove():void;
		function onPlayResign():void;
		function onPlayTimeout():void;
		function onGameOver():void;

		// ---------------------------------------------------------------------
		// Called by the game.

		/**
		 * @return
		 * The config of the current game.
		 * {
		 * 	nPlayers,
		 * 	moveSec,
		 * 	totalMin
		 * }
		 */
		function get baseConfig():Object;

		/**
		 * @return
		 * The extended config of the current game. If you want
		 * to use extended config, please define a custom config dialog.
		 */
		function get extendedConfig():Object;

		/**
		 * @return
		 * Nicks of players who initially joined the game (players can resign
		 * during the game).
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
		 * is not null. This is useful for WEGO games: players can change his decision
		 * if his opponent has not moved and timeout has not occured.
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
		 * "Action" means onNew/onMove()/onResign()/onTimeout(). The game calls this method
		 * to notify that game action processing has finished. The processing may take
		 * a lot of time, typically when the game uses physics engine.
		 *
		 * @param result
		 * OVER                The game is over.
		 * ANY                 Any player can make the next move.
		 * Non-negative number Index of the player who should make the next move.
		 */
		function onActionDone(result:int):void;

		/**
		 * For IGOUGO games, it is the index of the player who should make the next move.
		 */
		function get lastActionResult():int;

		/**
		 * Enqueue a move to the server.
		 */
		function enqueueMove(data:Object):void;

		// ---------------------------------------------------------------------------
		// Called by the config dialog.

		/**
		 * The config dialog calls to notify that the player has finished configurating
		 * a new game and is ready for other players to join.
		 */
		function config(baseConfig:Object, extendedConfig:Object):void;

		/**
		 * The config dialog calls to notify that the player has decided to join.
		 */
		function join():void;

		/**
		 * The config dialog calls to notify that the player has decided to unjoin.
		 */
		function unjoin():void;

		// ---------------------------------------------------------------------------
		// Utilities.

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
	}
}