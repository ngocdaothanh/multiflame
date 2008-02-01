package net.web20games.container.events {
	import flash.events.Event;

	public class ChatEvent extends Event {
		public static const CHAT:String = "CHAT";

		public var nick:String;
		public var message:String;

		public function ChatEvent(nick:String, message:String):void {
			super(CHAT);
			this.nick = nick;
			this.message = message;
		}
	}
}