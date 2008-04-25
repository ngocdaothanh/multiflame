package multiflame.container.events {
	import flash.events.Event;

	public class NewEvent extends Event {
		// Uses nick, baseConfig, extendedConfig
		public static const CONF:String    = "NEW_CONF";

		// Uses nick
		public static const JOIN:String    = "NEW_JOIN";

		// Uses nick
		public static const UNJOIN:String  = "NEW_UNJOIN";

		public static const TIMEOUT:String = "NEW_TIMEOUT";

		public var nick:String;
		public var baseConfig:Object;
		public var extendedConfig:Object;

		public function NewEvent(type:String):void {
			super(type);
		}
	}
}