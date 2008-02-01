package net.web20games.game {
	import flash.display.MovieClip;

	/**
	 * NOTE on "enabled" property:
	 *
	 * The enabled property of this movie clip is used to mark that player inputs
	 * are being enabled/disabled. For example, on mouse events, your game must
	 * check if enabled is true before further processing. During a game/match,
	 * this property is automatically set by the game container based on the returned
	 * value from onNewGame(), onMove(), onResign(), onTimeout(). Your game never have
	 * to set this property.
	 *
	 * NOTE on TweenLite and TweenFilterLite:
	 *
	 * Because this movie clip can be used at places other than the game container,
	 * do not use TweenLite or TweenFilterLite when not in game. If you need to
	 * ensure that TweenLite and TweenFilterLite are always available, please
	 * import gs.TweenLite and gs.TweenFilterLite in your game.
	 */
	public class Game extends MovieClip {
		/**
		 * Game classes.
		 */
		public static const CLASS_TURN_BASED:int = 0;
		public static const CLASS_BATCH:int      = 1;
		public static const CLASS_REALTIME:int   = 2;

		/**
		 * Results for each player.
		 */
		public static const P_NONE:int = -1;
		public static const P_LOST:int = 0;
		public static const P_DREW:int = 1;
		public static const P_WON:int  = 3;  // Do you play football? (No, not that American football :D)

		/**
		 * Results for each action (onMove/onResign/onTimeout).
		 */
		public static const A_OVER:int = -1;
		public static const A_ANY:int  = -2;

		// --------------------------------------------------------------------------

		public function Game():void {
			enabled = false;
		}

		/**
		 * @return true if this game instance is being used for judging game result.
		 */
		public function isJudging():Boolean {
			return _judging;
		}

		/**
		 * @return
		 * {
		 *   nPlayers,
		 *   moveSec,
		 *   totalMin
		 * }
		 */
		public final function get baseConfig():Object {
			return _baseConfig;
		}

		/**
		 * If you want to use extended config, please define a custom config dialog.
		 */
		public final function get extendedConfig():Object {
			return _extendedConfig;
		}

		/**
		 * @return Nicks of players who initially joined the game. Note that players
		 * can resigns during the game.
		 */
		public final function get nicks0():Array {
			return _nicks0;
		}

		/**
		 * @return Index of this player, negative if this player did not joined the game.
		 */
		public final function get indexMe():int {
			return _indexMe;
		}

		/**
		 * Use this method to update the result at each move.
		 *
		 * @param index  Index of the player to update.
		 * @param result P_LOST/P_DREW/P_WON
		 */
		public final function updateGameResult(index:int, result:int):void {
			_gameResult[index] = result;
		}

		/**
		 * "Action" means move/resign/timeout. Convenient for immediate-game, because
		 * it is the index of the player who should make the next move.
		 */
		public final function get lastActionResult():int {
			return _lastActionResult;
		}

		/**
		 * @return Array of results (P_NONE/P_LOST/P_DREW/P_WON) for each player.
		 */
		public final function get gameResult():Array {
			return _gameResult;
		}

		/**
		 * If your game want to display something to the players when the game is over,
		 * set it here.
		 */
		public final function set summary(value:String):void {
			_summary = value;
		}

		public final function get summary():String {
			return _summary;
		}

		// --------------------------------------------------------------------------

		/**
		 * Make a move.
		 */
		public final function move(data:Object):void {
			if (_container != null) {
				_container.move(data);
			}
		}

		/**
		 * Translate text on view.
		 */
		public final function _(id:String):String {
			return (_container == null)? id : _container._(id);
		}

		/**
		 * @return TweenLite class. See http://www.TweenLite.com.
		 */
		public final function get TweenLite():* {
			return (_container == null)? null : _container.TweenLite;
		}

		/**
		 * @return TweenFilerLite class. See http://www.TweenLite.com.
		 */
		public final function get TweenFilterLite():* {
			return (_container == null)? null : _container.TweenFilterLite;
		}

		// --------------------------------------------------------------------------

		/**
		 * @return Definition about this game:
		 * {
		 *   klass                        Game class, CLASS_XXX
		 *   nPlayersMin, nPlayersMax,    Number of players, should be >= 2
		 *   moveSecMin, moveSecMax,      [sec], = 0 for unlimited, not used for realtime game
		 *   totalMinMin, totalMinMax,    [min], = 0 for unlimited
		 * }
		 */
		public function get definition():Object {
			throw new Error("Your game must implement this method");
		}

		/**
		 * Called when the container has just been set.
		 *
		 * @return
		 * {
		 *   configDlg,    An instance of IConfigDlg (null to use the default)
		 *   introSprite   A sprite displaying game introduction (rule, trivia...)
		 * }
		 */
		protected function onContainerSet():Object {
			throw new Error("You must implement this method");
		}

		/**
		 * Called to initialize a new game. The following things have been set:
		 * * baseConfig and extendedConfig: according to the config dialog
		 * * gameResult: P_NONE for all players
		 *
		 * The model should examine baseConfig and extendedConfig (number of players...)
		 * to act properly.
		 *
		 * @param playedBack true if this player enters the game room when a game is
		 * taking place, and the game actions are being played back. In this case this
		 * method should not call move() or making any multimedia effects.
		 *
		 * @return
		 * A_ANY               Any player can make the first move.
		 * Non-negative number Index of the player who should make the first move,
		 *                     normally 0 for turn-based game.
		 */
		protected function onNewGame(playedBack:Boolean):int {
			throw new Error("You must implement this method");
		}

		/**
		 * Called by the game container to notify that a player (for immediate-mode
		 * game) or players (for batch-mode game) has/have moved. Will not be called
		 * if the game is over.
		 *
		 * This method should follow the following algorithm:
		 * 1. Validate inputs and return A_OVER if the validation failed. For
		 *    immediate-mode turn-based game, the model should check the index
		 *    parameter for the order of players, so that a certain player cannot
		 *    move successively. The result of the player who made the invalid move
		 *    should be P_LOST.
		 * 2. Actually make the moves if the validation passed.
		 * 3. Compute and use updateGameResult() to update results for players whose
		 *    results should be updated. If the returned value is A_OVER, every
		 *    player must have result.
		 *
		 * @param timestamp Seconds from the game start.
		 *
		 * @param moves [index, data, index, data...]. index: index of the player who
		 * made the move. data: data representing the move.
		 *
		 * For turn-based and realtime game: there's only one (index, data) pair.
		 *
		 * For batch game: (1) If a player did not move within baseConfig.moveSec,
		 * his data is null. (2) If a player moved then resigned within
		 * baseConfig.moveSec, his (index, data) is not included in moves.
		 *
		 * @param playedBack Same as onNewGame().
		 *
		 * @return
		 *   A_OVER                 The game is over.
		 *   A_ANY                  Any player can make the next move.
		 *   Non-negative number    Index of the player who should make the next move.
		 */
		protected function onMove(timestamp:Number, moves:Array, playedBack:Boolean):int {
			throw new Error("You must implement this method");
		}

		/**
		 * Called to notify that a player has resigned.
		 *
		 * This method should follow the following algorithm:
		 * 1. Reject the player who has resigned.
		 * 2. Compute and use updateGameResult() to update results for players whose
		 *    results should be updated. If the returned value is A_OVER, every
		 *    player must have result.
		 *
		 * @param timestamp Seconds from the game start.
		 *
		 * @param index Index of the player who has resigned. For game with more than
		 * 2 players, it is possible that a player resigns without making the game
		 * over. If a player has won then resigns, his result will still be "won".
		 *
		 * @param playedBack Same as onNewGame().
		 *
		 * @return Same as onMove().
		 */
		protected function onResign(timestamp:Number, index:int, playedBack:Boolean):int {
			throw new Error("You must implement this method");
		}

		/**
		 * Not called for batch game. For batch game, when a player timed out, the
		 * server would make him resign.
		 *
		 * Called to notify that a player's time (one move or total) has passed.
		 *
		 * This method should follow the following algorithm:
		 * 1. If timedOut is false, fine the player who has requested this timeout
		 *    action by updating his result to P_LOST.
		 * 3. If timedOut is true, compute and update results. If the returned value
		 *    is A_OVER, every player must have result.
		 *
		 * @param timestamp Seconds from the game start.
		 *
		 * @param timedOut true if timeout has really occured.
		 *
		 * @param index If timedOut is true, it is the index of the player who caused
		 * the timeout. If timedOut is false, it is the index of the player who requested
		 * this timeout action.
		 *
		 * @param playedBack Same as onNewGame().
		 *
		 * @return Same as onMove().
		 */
		protected function onTimeout(timestamp:Number, timedOut:Boolean, index:int, playedBack:Boolean):int {
			throw new Error("You must implement this method");
		}

		// --------------------------------------------------------------------------

		private var _judging:Boolean;
		private var _baseConfig:Object;
		private var _extendedConfig:Object;

		private var _container:IContainer;

		private var _nicks0:Array;
		private var _indexMe:int;

		private var _lastActionResult:int;
		private var _gameResult:Array;
		private var _summary:String;

		public final function onContainerSetWrapper(container:IContainer, judging:Boolean):Object {
			_container = container;
			_judging = judging;
			return onContainerSet();
		}

		// Wrappers are needed for lastActionResult to work.

		public final function onNewGameWrapper(baseConfig:Object, extendedConfig:Object,
				nicks0:Array, indexMe:int, playedBack:Boolean):int {
			_baseConfig = baseConfig;
			_extendedConfig = extendedConfig;
			_nicks0 = nicks0;
			_indexMe = indexMe;

			// Game result
			_gameResult = new Array(baseConfig.nPlayers);
			for (var i:int = 0; i < baseConfig.nPlayers; i++) {
				_gameResult[i] = P_NONE;
			}

			_lastActionResult = onNewGame(playedBack);
			return _lastActionResult;
		}

		public final function onMoveWrapper(timestamp:Number, moves:Array, playedBack:Boolean):int {
			if (_lastActionResult != A_OVER) {
				_lastActionResult = onMove(timestamp, moves, playedBack);
			}
			return _lastActionResult;
		}

		public final function onResignWrapper(timestamp:Number, index:int, playedBack:Boolean):int {
			if (_lastActionResult != A_OVER) {
				_lastActionResult = onResign(timestamp, index, playedBack);
			}
			return _lastActionResult;
		}

		public final function onTimeoutWrapper(timestamp:Number, timedOut:Boolean, index:int, playedBack:Boolean):int {
			if (_lastActionResult != A_OVER) {
				_lastActionResult = onTimeout(timestamp, timedOut, index, playedBack);
			}
			return _lastActionResult;
		}
	}
}