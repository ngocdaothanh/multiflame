package multiflame.game {
	public class Constants {
		/**
		 * Game types, see http://en.wikipedia.org/wiki/Turn-based_game
		 */
		public static const IGOUGO:int   = 0;
		public static const WEGO:int     = 1;
		public static const REALTIME:int = 2;

		/**
		 * Results for each player.
		 */
		public static const NONE:int = 0;
		public static const LOST:int = 1;
		public static const DREW:int = 2;
		public static const WON:int  = 3;

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