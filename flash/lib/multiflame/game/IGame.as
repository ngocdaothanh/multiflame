package multiflame.game {
	/**
	 * Instance of this interface is decoupled with instance of IContainer. As
	 * a result, to know about the game environment, instance of this interface must
	 * always refer to instance of IContainer.
	 */
	public interface IGame {
		/**
		 * @return
		 * {
		 * 	nPlayersMin, nPlayersMax,    Number of players, should be >= 2
		 * 	moveSecMin, moveSecMax,      [sec], = 0 for unlimited, not used for realtime game
		 * 	totalMinMin, totalMinMax     [min], = 0 for unlimited
		 * }
		 */
		function get baseConfigRange():Object;

		/**
		 * Utilities like translation, TweenLite and TweenFilterLite are not available
		 * until the game container has been set. Because this movie clip can be used
		 * at places other than the game container, if you need to ensure that
		 * TweenLite and TweenFilterLite are always available, please
		 * import gs.TweenLite and gs.TweenFilterLite in your game.
		 *
		 * @return
		 * {
		 * 	type,          Game type, see Constants.as
		 * 	configDlg,     An instance of IConfigDlg, null to use the default
		 * 	introSprite    A sprite displaying game introduction (rule, trivia...), null for none
		 * }
		 */
		function setContainer(container:IContainer):Object;

		/**
		 * Called to ask that the game must enable/disable user inputs.
		 */
		function set enabled(value:Boolean):void;

		/**
		 * @return
		 * The game snapshot, from which the game can be restored on other machines.
		 */
		function get snapshot():Object;

		/**
		 * The game container calls to ask the game to prepare a new game. 
		 *
		 * The game should examine the container for baseConfig, extendedConfig,
		 * gameSnapshot, indexMe...  to act properly. If snapshot is not null, it means
		 * that the player logs in when the game has started. In this case the game state
		 * must be restored.
		 *
		 * After processing, IContainer#onActionResult() must be called to notify that
		 * the processing has finished.
		 */
		function onNewGame(snapshot:Object):void;

		/**
		 * Called by the game container to notify that a player (for immediate-mode
		 * game) or players (for batch-mode game) has/have moved.
		 *
		 * This method should follow the following algorithm:
		 * 1. Validate inputs. For immediate-mode turn-based game, the model should
		 *    check the index parameter for the order of players, so that a certain player
		 *    cannot move successively. If validation fails, use updateGameResult() to
		 *    update the result of the player who makes the move to P_LOST.
		 * 2. Actually make the moves if the validation passed.
		 * 3. Compute and update IContainer#gameResult if neccessary.
		 * 4. Call IContainer#onActionResult() to notify that the processing has finished.
		 *
		 * @param timestamp
		 * Seconds from the game start.
		 *
		 * @param moves
		 * [index, data, index, data...].
		 * index: index of the player who made the move.
		 * data: data representing the move.
		 *
		 * For turn-based and realtime game: there's only one (index, data) pair.
		 *
		 * For batch game: (1) If a player did not move within baseConfig.moveSec,
		 * his data is null. (2) If a player moved then resigned within
		 * baseConfig.moveSec, his (index, data) is not included in moves.
		 */
		function onMove(timestamp:Number, moves:Array):void;

		/**
		 * Called to notify that a player has resigned.
		 *
		 * This method should follow the following algorithm:
		 * 1. Reject the player who has resigned.
		 * 2. Compute and update IContainer#gameResult if neccessary.
		 * 3. Call IContainer#onActionResult() to notify that the processing has finished.
		 *
		 * @param timestamp
		 * Seconds from the game start.
		 *
		 * @param index
		 * Index of the player who has resigned. For game with more than 2 players,
		 * it is possible that a player resigns without making the game
		 * over. If a player has won then resigns, his result will still be "won".
		 */
		function onResign(timestamp:Number, index:int):void;

		/**
		 * Called to notify that a player's time (one move or total) has passed.
		 * Not called for batch game.
		 *
		 * This method should follow the following algorithm:
		 * 1. If timedOut is false, set IContainer#gameResult[index] = LOST.
		 * 2. If timedOut is true, compute and update IContainer#gameResult if neccessary.
		 * 3. Call IContainer#onActionResult() to notify that the processing has finished.
		 *
		 * @param timestamp
		 * Seconds from the game start.
		 *
		 * @param timedOut
		 * true if timeout has really occured.
		 *
		 * @param index
		 * If timedOut is true, it is the index of the player who caused the timeout.
		 * If timedOut is false, it is the index of the player who requested this action.
		 */
		function onTimeout(timestamp:Number, timedOut:Boolean, index:int):void;
	}
}