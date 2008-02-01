package net.web20games.container.events {
	import flash.events.Event;

	public class RoomEnterLeaveEvent extends Event {
		// This player has entered a room
		public static const ENTER_ME:String             = "ENTER_ME";

		// Other player has entered a room while this player is in the lobby
		public static const ENTER_OTHER_ME_LOBBY:String = "ENTER_OTHER_ME_LOBBY";

		// Other player has entered a room while this player is in that room
		public static const ENTER_OTHER_ME_ROOM:String  = "ENTER_OTHER_ME_ROOM";

		public static const LEAVE_ME:String             = "LEAVE_ME";
		public static const LEAVE_OTHER_ME_LOBBY:String = "LEAVE_OTHER_ME_LOBBY";
		public static const LEAVE_OTHER_ME_ROOM:String  = "LEAVE_OTHER_ME_ROOM";

		// ENTER_ME: [state, nicks, baseConfig, extendedConfig, playNicks0, moves]
		// LEAVE_ME: [[nicks in lobby], [nicks in room0], [nicks in room1]...]
		public var snapshot:Array;

		// ENTER_OTHER_ME_LOBBY, LEAVE_OTHER_ME_LOBBY
		public var iroom:int;

		// ENTER_OTHER_ME_LOBBY, ENTER_OTHER_ME_ROOM,
		// LEAVE_OTHER_ME_LOBBY, LEAVE_OTHER_ME_ROOM
		public var nick:String;

		public function RoomEnterLeaveEvent(type:String):void {
			super(type);
		}
	}
}