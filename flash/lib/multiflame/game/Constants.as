package multiflame.game {
	public class Constants {
		public static const WEB_SITE:String = "web20games.net";

		/**
		 * Game classes.
		 */
		public static const TURN_BASED:int = 0;
		public static const BATCH:int      = 1;
		public static const REALTIME:int   = 2;

		/**
		 * Results for each player.
		 */
		public static const NONE:int = -1;
		public static const LOST:int = 0;
		public static const DREW:int = 1;
		public static const WON:int  = 3;  // Just like soccer

		/**
		 * Play actions.
		 */
		public static const MOVE:int    = 0;
		public static const RESIGN:int  = 1;
		public static const TIMEOUT:int = 2;

		/**
		 * Results for each action (onMove/onResign/onTimeout).
		 */
		public static const OVER:int = -1;
		public static const ANY:int  = -2;

		/**
		 * To get a random number in (0..limit - 1), call
		 * IContainer#enqueueMove(UTIL_RANDOM, limit).
		 * onMove will be called later with the parameter:
		 * [UTIL_RANDOM, limit, random number]
		 */
		public static const UTIL_RANDOM  = 'random';

		/**
		 * To get an array of random numbers in (0..limit - 1), call
		 * IContainer#enqueueMove(UTIL_SHUFFLE, limit).
		 * onMove will be called later with the parameter:
		 * [UTIL_SHUFFLE, limit, [random numbers]]
		 */
		public static const UTIL_SHUFFLE = 'shuffle';
	}
}