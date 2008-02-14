package multiflame.game {
	public class Constants {
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
	}
}