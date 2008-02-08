package net.web20games.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import fl.controls.*;

	import gs.TweenLite;

	import net.web20games.container.events.*;
	import net.web20games.utils.*;

	public class ChatDlg extends MovieClip {
		private var _channel:Channel;
		private var _lastChatNick:String;  // Nick of the player who sent the last message
		private var _dragging:Boolean;
		
		public function ChatDlg():void {
			_channel = Channel.instance;
			_lastChatNick = "";

			_chatRadio.addEventListener(Event.CHANGE, onChatRadioChange);
			_closeBtn.addEventListener(MouseEvent.CLICK, onCloseClick);

			_chatOutput.verticalScrollPolicy = ScrollPolicy.ON;
			_playerList.verticalScrollPolicy = ScrollPolicy.ON;
			removeChild(_playerList);

			_chatInput.border = true;
			_chatInput.addEventListener(KeyboardEvent.KEY_DOWN, onChatInputKeyDown);

			addEventListener(Event.ADDED, onAdded);

			// Draging
			addEventListener(MouseEvent.MOUSE_DOWN, onMouseDown);
			addEventListener(MouseEvent.MOUSE_UP, onMouseUp);

			addEventListener(MouseEvent.MOUSE_OUT, onMouseOut);
			addEventListener(MouseEvent.MOUSE_OVER, onMouseOver);

			_channel.addEventListener(LoginoutEvent.LOGIN_ME, onLoginMe);
			_channel.addEventListener(LoginoutEvent.LOGIN, onLoginout);
			_channel.addEventListener(LoginoutEvent.LOGOUT, onLoginout);
			_channel.addEventListener(CloseEvent.WILL_CLOSE, onClose);
			_channel.addEventListener(RoomEnterLeaveEvent.ENTER_ME, onRoomEnterMe);
			_channel.addEventListener(RoomEnterLeaveEvent.ENTER_OTHER_ME_LOBBY, onRoomEnterOtherMeLobby);
			_channel.addEventListener(RoomEnterLeaveEvent.ENTER_OTHER_ME_ROOM, onRoomEnterOtherMeRoom);
			_channel.addEventListener(RoomEnterLeaveEvent.LEAVE_OTHER_ME_ROOM, onRoomLeaveOtherMeRoom);
			_channel.addEventListener(ChatEvent.CHAT, onChat);
		}
		
		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}
			_chatRadio.label = _("Chat");
			_chatRadio.selected = true;
			_playerList.x = _chatOutput.x;
			_playerList.y = _chatOutput.y;
			onChatRadioChange(null);
		}

		private function onChatRadioChange(event:Event):void {
			if (event == null || event.target == _chatRadio || event.target == _playersRadio) {
				if (_chatRadio.selected) {
					if (!contains(_chatOutput)) {
						addChild(_chatOutput);
						removeChild(_playerList);
					}
				} else {
					if (!contains(_playerList)) {
						addChild(_playerList);
						removeChild(_chatOutput);
					}
				}
			}
		}

		private function onCloseClick(event:MouseEvent):void {
			parent.removeChild(this);
		}

		// ---------------------------------------------------------------------------

		private function onMouseDown(event:MouseEvent):void {
			parent.setChildIndex(this, parent.numChildren - 1);
			if (event.target != _chatInput &&
					!_chatOutput.contains(event.target as DisplayObject) &&
					!_playerList.contains(event.target as DisplayObject)) {
				startDrag();
				_dragging = true;
			}
		}
		
		private function onMouseUp(event:MouseEvent):void {
			if (_dragging) {
				stopDrag();
				_dragging = false;
			}
		}

		// ---------------------------------------------------------------------------

		private function onMouseOut(event:MouseEvent):void {
			if (!_dragging) {
				TweenLite.to(this, 0.5, {scaleX: 0.5, scaleY: 0.5});
			}
		}

		private function onMouseOver(event:MouseEvent):void {
			TweenLite.to(this, 0.5, {scaleX: 1, scaleY: 1});
		}

		// ---------------------------------------------------------------------------

		private function onChatInputKeyDown(event:KeyboardEvent):void {
			if (event.keyCode == 13) {
				if (_chatInput.text.match(/[^\s]/) != null) {
					_channel.chat(_chatInput.text);
				}
				_chatInput.text = "";
			}
		}

		// --------------------------------------------------------------------------

		// Set player list when the player first login
		private function onLoginMe(event:LoginoutEvent):void {
			if (event.code == LoginoutEvent.OK) {
				_chatOutput.htmlText = "";
				refreshPlayerList();
			}
		}

		private function onLoginout(event:LoginoutEvent):void {
			if (event.type == LoginoutEvent.LOGIN) {
				addChat(StringUtil.substitute(_("<b>{0}</b> has logged in"), event.nick));
			} else {
				addChat(StringUtil.substitute(_("<b>{0}</b> has logged out"), event.nick));
			}
			refreshPlayerList();
		}

		private function onClose(event:CloseEvent):void {
			if (event.type == CloseEvent.WILL_CLOSE) {
				addChat(_("The server will close in a few minutes for maintenance, sorry for this inconvenience"));
			}
		}

		private function onRoomEnterMe(event:RoomEnterLeaveEvent):void {
			refreshPlayerList();
		}

		private function onRoomEnterOtherMeLobby(event:RoomEnterLeaveEvent):void {
			addChat(StringUtil.substitute(_('{0} has entered room #{1}'), event.nick, event.iroom + 1));
			refreshPlayerList();
		}

		private function onRoomEnterOtherMeRoom(event:RoomEnterLeaveEvent):void {
			addChat(StringUtil.substitute(_('{0} has entered'), event.nick));
			refreshPlayerList();
		}

		private function onRoomLeaveOtherMeRoom(event:RoomEnterLeaveEvent):void {
			addChat(StringUtil.substitute(_('{0} has left'), event.nick));
			refreshPlayerList();
		}

		private function onChat(event:ChatEvent):void {		
			if (event.nick != _lastChatNick) {
				addChat("<b>" + event.nick + "</b>: " + event.message, false);
				_lastChatNick = event.nick;
			} else {
				addChat(event.message, false);
			}
		}

		// --------------------------------------------------------------------------

		private function addChat(msg:String, system:Boolean = true) {
			if (system) {
				_chatOutput.htmlText += '<p><font color="#FF0000">' + msg + '</font></p>';
			} else {
				_chatOutput.htmlText += "<p>" + msg + "</p>";
			}
			_chatOutput.verticalScrollPosition = _chatOutput.maxVerticalScrollPosition;
		}

		private function refreshPlayerList() {
			_playerList.htmlText = "";
			for (var i:int = 0; i < _channel.nicks.length; i++) {
				_playerList.htmlText += "<p>" + _channel.nicks[i] + "</p>";
			}
			_playersRadio.label = StringUtil.substitute(_('Players ({0})'), _channel.nicks.length);
		}

		//---------------------------------------------------------------------------

		private function _(id:String):String {
			return LoadScreen.clientGetText._(id);
		}
	}
}