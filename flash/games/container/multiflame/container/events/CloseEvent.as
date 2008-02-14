package multiflame.container.events {
	import flash.events.Event;

	// Server event after this client has logged in.
	public class CloseEvent extends Event {
		// The server will be closed down in 5 minutes (for upgrading, moving...)
		public static const WILL_CLOSE:String = "WILL_CLOSE";

		public static const CLOSED:String     = "CLOSED";

		public function CloseEvent(type:String):void {
			super(type);
		}
	}
}