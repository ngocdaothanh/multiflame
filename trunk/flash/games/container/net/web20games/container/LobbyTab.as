package net.web20games.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import fl.events.ListEvent;
	
	import net.web20games.container.events.*;

	public class LobbyTab extends Sprite {
		private var _channel:Channel;

		public function LobbyTab():void {
			addEventListener(Event.ADDED, onAdded);
			_newRoomBtn.addEventListener(MouseEvent.CLICK, onNewRoomClick);
			_roomList.addEventListener(fl.events.ListEvent.ITEM_CLICK, onRoomClick);

			_channel = Channel.instance;
			_channel.addEventListener(LoginoutEvent.LOGIN_ME, onLoginMe);
			_channel.addEventListener(RoomEnterLeaveEvent.ENTER_OTHER_ME_LOBBY, onRoomEnterOtherMeLobby);
			_channel.addEventListener(RoomEnterLeaveEvent.LEAVE_ME, onRoomLeaveMe);
			_channel.addEventListener(RoomEnterLeaveEvent.LEAVE_OTHER_ME_LOBBY, onRoomLeaveOtherMeLobby);
		}
		
		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}
		}

		private function onNewRoomClick(event:MouseEvent):void {
			_channel.room_enter(-1);
		}

		private function onRoomClick(event:ListEvent):void {
			_channel.room_enter(event.index);
		}

		// ---------------------------------------------------------------------------

		private function onLoginMe(event:LoginoutEvent):void {
			if (event.code == LoginoutEvent.OK) {
				_roomList.removeAll();

				var a:Array = event.snapshot;
				for (var i:int = 1; i < a.length; i++) {
					var nicks:Array = a[i];
					_roomList.addItem({label: renderNicks(i - 1, nicks), data: {nicks: nicks}});
				}
			}
		}

		private function onRoomEnterOtherMeLobby(event:RoomEnterLeaveEvent):void {
			var nicks:Array;
			if (event.iroom == _roomList.length) {
				_roomList.addItem({label: renderNicks(event.iroom, [event.nick]),
					data: {nicks: [event.nick]}});
			} else {
				var item:Object = _roomList.getItemAt(event.iroom);
				item.data.nicks.push(event.nick);
				item.label = renderNicks(event.iroom, item.data.nicks);
				_roomList.replaceItemAt(item, event.iroom);
			}
		}

		private function onRoomLeaveMe(event:RoomEnterLeaveEvent):void {
			_roomList.removeAll();

			var a:Array = event.snapshot;
			for (var i:int = 1; i < a.length; i++) {
				var nicks:Array = a[i];
				_roomList.addItem({label: renderNicks(i, nicks), data: {nicks: nicks}});
			}
		}

		private function onRoomLeaveOtherMeLobby(event:RoomEnterLeaveEvent):void {
			var item:Object = _roomList.getItemAt(event.iroom);
			var nicks:Array = item.data.nicks;
			if (nicks.length == 1) {
				_roomList.removeItemAt(event.iroom);
			} else {
				nicks.splice(nicks.indexOf(event.nick), 1);
				item.label = renderNicks(event.iroom, nicks);
				_roomList.replaceItemAt(item, event.iroom);
			}
		}

		private function renderNicks(iroom:int, nicks:Array):String {
			var ret:String;

			ret = "#" + (iroom + 1) + " (" + nicks.length + ") ";
			ret += nicks.join("  ");
			return ret;
		}

		// --------------------------------------------------------------------------

		private function _(id:String):String {
			return LoadScreen.clientGetText._(id);
		}
	}
}