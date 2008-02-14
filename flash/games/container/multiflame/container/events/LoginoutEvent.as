package multiflame.container.events {
	import flash.events.Event;

	public class LoginoutEvent extends Event {
		public static const LOGIN_ME:String = "LOGIN_ME";
		public static const LOGIN:String    = "LOGIN";
		public static const LOGOUT:String   = "LOGOUT";

		// Login result (LOGIN_ME) ---------------------------------------------------

		public static const OK:int                          = 0;
		public static const CONNECTION_ERROR:int            = 1;
		public static const WRONG_CAPTCHA:int               = 2;
		public static const DIFFERENT_CONTAINER_VERSION:int = 3;
		public static const DIFFERENT_GAME_VERSION:int      = 4;
		public static const DUPLICATE_NICK:int              = 5;
		public static const OLD_CONTAINER_VERSION:int       = 6;
		public static const NO_GAME:int                     = 7;
		public static const OLD_GAME_VERSION:int            = 8;
		public static const WRONG_NICK_OR_PASSWORD:int      = 9;
		public static const NOT_FRIENDS:int                 = 10;
		public static const REDIRECT:int                    = 11;

		public var code:int;

		// Only meaningful for OK
		// [[nicks in lobby], [nicks in room0], [nicks in room1]...]
		public var snapshot:Array;

		// End of login result (LOGIN_ME) --------------------------------------------

		public var nick:String;

		public function LoginoutEvent(type:String, nick:String):void {
			super(type);
			this.nick = nick;
		}
	}
}
