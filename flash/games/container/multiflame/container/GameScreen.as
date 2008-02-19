package multiflame.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import flash.net.*;
	import fl.controls.*;
	
	import com.adobe.webapis.gettext.GetText;
	import gs.TweenFilterLite;	

	import multiflame.container.events.*;
	import multiflame.game.Constants;

	public class GameScreen extends Sprite {
		public static const TAB_HEIGHT:int = 30 + 1;

		private var _channel:Channel;

		private var _loginTab:LoginTab;
		private var _lobbyTab:LobbyTab;
		private var _roomTab:RoomTab;

		private var _introTab:IntroTab;
		private var _embedTab:EmbedTab;

		private var _chatDlg:ChatDlg;
		private var _timeoutSB:TimeoutStatusBar;

		public function GameScreen():void {
			addEventListener(Event.ADDED, onAdded);
			_loginLobbyRoomTabBtn.addEventListener(MouseEvent.MOUSE_DOWN, onLoginLobbyRoomTabBtnDown);
			_introTabBtn.addEventListener(MouseEvent.MOUSE_DOWN, onIntroTabBtnDown);
			_embedTabBtn.addEventListener(MouseEvent.MOUSE_DOWN, onEmbedTabBtnDown);

			_chatDlg = new ChatDlg();
			_chatDlg.x = 500 - 220;
			_chatDlg.y = 30;
			_chatBtn.addEventListener(MouseEvent.CLICK, onChatBtnClick);
			
			_timeoutSB = new TimeoutStatusBar(_timeoutLbl);
			_timeoutSB.setStatus("");
			_timeoutSB.addEventListener(TimeoutStatusBar.ABOUT_MOVE_TIMEOUT, onAboutMoveTimeout);
			_timeoutSB.addEventListener(TimeoutStatusBar.TIMEOUT, onTimeout);

			_leaveBtn.addEventListener(MouseEvent.CLICK, onLeaveClick);

			_channel = Channel.instance;
			_channel.addEventListener(LoginoutEvent.LOGIN_ME, onLoginMe);
			_channel.addEventListener(CloseEvent.CLOSED, onClosed);
			_channel.addEventListener(RoomEnterLeaveEvent.ENTER_ME, onRoomEnterMe);
			_channel.addEventListener(RoomEnterLeaveEvent.LEAVE_ME, onRoomLeaveMe);
			_channel.addEventListener(ChatEvent.CHAT, onChat);
			_channel.addEventListener(NewEvent.CONFIG, onNewConfig);
			_channel.addEventListener(NewEvent.JOIN, onNewJoin);
			_channel.addEventListener(NewEvent.UNJOIN, onNewUnjoin);
			_channel.addEventListener(NewEvent.TIMEOUT, onNewTimeout);
			_channel.addEventListener(PlayEvent.MOVE, onPlayAction);
			_channel.addEventListener(PlayEvent.RESIGN, onPlayAction);
			_channel.addEventListener(PlayEvent.TIMEOUT, onPlayAction);
			_channel.addEventListener(PlayEvent.ACTION_RESULT, onPlayActionResult);

			_loginTab = new LoginTab();
			_loginTab.y = TAB_HEIGHT;

			_lobbyTab = new LobbyTab();
			_lobbyTab.y = TAB_HEIGHT;

			_roomTab = RoomTab.instance;
			_roomTab.y = TAB_HEIGHT;

			_introTab = new IntroTab();
			_introTab.y = TAB_HEIGHT;

			_embedTab = new EmbedTab();
			_embedTab.y = TAB_HEIGHT;
		}
		
		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}

			_loginLobbyRoomTabBtn.label = _("Login");
			_introTabBtn.label = _("Introduction");
			_embedTabBtn.label = _("Embed");
			_leaveBtn.label   = _("Leave");
			_leaveBtn.enabled = false;

			_loginLobbyRoomTabBtn.off = false;
			_introTabBtn.off = true;
			_embedTabBtn.off = true;
			removeChild(_chatBtn);

			if (!contains(_loginTab)) {
				addChild(_loginTab);
			}
			if (contains(_lobbyTab)) {
				removeChild(_lobbyTab);
			}
			if (contains(_roomTab)) {
				removeChild(_roomTab);
			}
			if (contains(_introTab)) {
				removeChild(_introTab);
			}

			// Hide fullscreen check box if the variable is not set
			var vars:Object = LoaderInfo(this.root.loaderInfo).parameters;
			if (vars == null || String(vars["allowFullScreen"]) != "true") {
				removeChild(_fullScreenCheckBox);
			} else {
				_fullScreenCheckBox.label = _("Fullscreen");
				_fullScreenCheckBox.addEventListener(MouseEvent.CLICK, onFullScreenClick);
				stage.addEventListener(FullScreenEvent.FULL_SCREEN, onFullScreen);
			}
		}

		// --------------------------------------------------------------------------

		private function onLoginLobbyRoomTabBtnDown(event:MouseEvent):void {
			if (!_loginLobbyRoomTabBtn.off) {
				return;
			}
			_loginLobbyRoomTabBtn.off = false;
			_introTabBtn.off = true;
			_embedTabBtn.off = true;

			var tab:Sprite;
			if (_channel.state == Channel.NOT_LOGGED_IN) {
				tab = _loginTab;
			} else if (_channel.state == Channel.IN_LOBBY) {
				tab = _lobbyTab;
			} else {
				tab = _roomTab;
			}

			if (!contains(tab)) {
				var tabs:Array = [_loginTab, _lobbyTab, _roomTab];
				for (var i:int = 0; i < tabs.length; i++) {
					if (tab != tabs[i] && contains(tabs[i])) {
						removeChild(tabs[i]);
					}
				}

				addChild(tab);

				if (contains(_chatDlg)) {
					setChildIndex(_chatDlg, numChildren - 1);
				}
			}

			if (contains(_introTab)) {
				removeChild(_introTab);
			}
			if (contains(_embedTab)) {
				removeChild(_embedTab);
			}
		}
		
		private function onIntroTabBtnDown(event:MouseEvent):void {
			if (!_introTabBtn.off) {
				return;
			}
			_loginLobbyRoomTabBtn.off = true;
			_introTabBtn.off = false;
			_embedTabBtn.off = true;

			if (contains(_loginTab)) {
				removeChild(_loginTab);
			}
			if (contains(_lobbyTab)) {
				removeChild(_lobbyTab);
			}
			if (contains(_roomTab)) {
				removeChild(_roomTab);
			}
			if (!contains(_introTab)) {
				addChild(_introTab);
				if (contains(_chatDlg)) {
					setChildIndex(_chatDlg, numChildren - 1);
				}
			}
			if (contains(_embedTab)) {
				removeChild(_embedTab);
			}
		}

		private function onEmbedTabBtnDown(event:MouseEvent):void {
			if (!_embedTabBtn.off) {
				return;
			}
			_loginLobbyRoomTabBtn.off = true;
			_introTabBtn.off = true;
			_embedTabBtn.off = false;

			if (contains(_loginTab)) {
				removeChild(_loginTab);
			}
			if (contains(_lobbyTab)) {
				removeChild(_lobbyTab);
			}
			if (contains(_roomTab)) {
				removeChild(_roomTab);
			}
			if (contains(_introTab)) {
				removeChild(_introTab);
			}
			if (!contains(_embedTab)) {
				addChild(_embedTab);
				if (contains(_chatDlg)) {
					setChildIndex(_chatDlg, numChildren - 1);
				}
			}
		}

		private function onChatBtnClick(event:MouseEvent):void {
			if (_channel.state <= Channel.NOT_LOGGED_IN) {
				return;
			}
			if (contains(_chatDlg)) {
				removeChild(_chatDlg);
			} else {
				addChild(_chatDlg);
			}
		}

		// --------------------------------------------------------------------------
		
		private function onLoginMe(event:LoginoutEvent):void {
			if (event.code == LoginoutEvent.OK) {
				if (contains(_loginTab)) {
					removeChild(_loginTab);
				} else {
					removeChild(_introTab);
				}
				addChild(_lobbyTab);
				_loginLobbyRoomTabBtn.label = _("Lobby");
				_loginLobbyRoomTabBtn.off = false;
				_introTabBtn.off = true;

				if (!contains(_chatBtn)) {
					addChild(_chatBtn);
				}
			}
		}

		private function onClosed(event:CloseEvent):void {
			if (event.type == CloseEvent.CLOSED) {
				removeChild(_chatBtn);
				if (contains(_chatDlg)) {
					removeChild(_chatDlg);
				}
				
				if (contains(_lobbyTab)) {
					removeChild(_lobbyTab);
				}
				if (contains(_roomTab)) {
					removeChild(_roomTab);
				}
				if (contains(_introTab)) {
					removeChild(_introTab);
				}
				addChild(_loginTab);
				_loginLobbyRoomTabBtn.label = _("Lobby");
				_loginLobbyRoomTabBtn.off = false;
				_introTabBtn.off = true;
				
				_timeoutSB.setStatus("");
				_leaveBtn.enabled = false;
			}
		}

		// --------------------------------------------------------------------------

		private function onRoomEnterMe(event:RoomEnterLeaveEvent):void {
			if (contains(_lobbyTab)) {
				removeChild(_lobbyTab);
			} else {
				removeChild(_introTab);
			}
			addChild(_roomTab);
			_loginLobbyRoomTabBtn.label = _("Room");
			_loginLobbyRoomTabBtn.off = false;
			_introTabBtn.off = true;

			// The chat dialog is behind the game sprite
			if (contains(_chatDlg)) {
				setChildIndex(_chatDlg, numChildren - 1);
			}

			_leaveBtn.enabled = true;
		}

		private function onRoomLeaveMe(event:RoomEnterLeaveEvent):void {
			if (contains(_roomTab)) {
				removeChild(_roomTab);
			} else {
				removeChild(_introTab);
			}
			addChild(_lobbyTab);
			_loginLobbyRoomTabBtn.label = _("Lobby");
			_loginLobbyRoomTabBtn.off = false;
			_introTabBtn.off = true;

			if (contains(_chatDlg)) {
				setChildIndex(_chatDlg, numChildren - 1);
			}

			_leaveBtn.enabled = false;
		}

		// --------------------------------------------------------------------------

		private function onChat(event:ChatEvent):void {
			if (!contains(_chatDlg)) {
				TweenFilterLite.to(_chatBtn, 0.5, {type: "Color", colorize: 0xF2DB0D, amount: 1});
				TweenFilterLite.to(_chatBtn, 0.5, {delay: 0.6, type: "Color", amount: 1, overwrite: false});
			}
		}

		// --------------------------------------------------------------------------

		private function onNewConfig(event:NewEvent):void {
			_timeoutSB.setStatus(_("New game"), Channel.NEW_TIMEOUT);
		}
		
		private function onNewJoin(event:NewEvent):void {
			if (_channel.state == Channel.PLAY) {
				_timeoutSB.stop();

				if (_channel.playNicks0.length == _channel.baseConfig.nPlayers &&
						_channel.playNicks0.indexOf(_channel.nick) >= 0) {
					_leaveBtn.label = _("Resign");
				}
			}
		}

		private function onNewUnjoin(event:NewEvent):void {
			if (_channel.state == Channel.NEWABLE) {
				_timeoutSB.setStatus("");
			}
		}

		private function onNewTimeout(event:NewEvent):void {
			_timeoutSB.setStatus("");
		}

		// --------------------------------------------------------------------------

		public function onPlayAction(event:PlayEvent):void {
			_timeoutSB.setStatus("");
		}

		public function onPlayActionResult(event:PlayEvent):void {
			var text:String;
			if (event.actionResult >= 0) {
				text = _channel.playNicks0[event.actionResult];
			} else if (event.actionResult == Constants.ANY) {
				text = "";
			} else {
				text = _("Game over");
				_leaveBtn.label = _("Leave");
			}
			_timeoutSB.setStatus(text, event.moveSecLeft, event.totalSecLeft);
		}
		
		// --------------------------------------------------------------------------

		private function onAboutMoveTimeout(event:Event):void {
			if (_channel.state == Channel.PLAY &&
					_channel.playNicks.indexOf(_channel.nick) >= 0 &&
					RoomTab.instance.enqueueDefaultMove()) {
				_timeoutSB.setStatus("");
			}
		}

		private function onTimeout(event:Event):void {
			if (_channel.state == Channel.NEW) {
				_channel.newTimeout();
				_timeoutSB.setStatus("");
			} else if (_channel.state == Channel.PLAY) {
				_channel.playTimeout();
			}
		}

		private function onFullScreenClick(event:MouseEvent):void {
			stage.displayState = _fullScreenCheckBox.selected ?
				StageDisplayState.FULL_SCREEN :
				StageDisplayState.NORMAL;
		}

		private function onFullScreen(event:FullScreenEvent):void {
			_fullScreenCheckBox.selected = event.fullScreen;
		}

		private function onLeaveClick(event:MouseEvent):void {
			if (_leaveBtn.label == _("Resign")) {
				_channel.playResign();
				_leaveBtn.label = _("Leave");
			} else {
				_channel.roomLeave();
			}
		}
		
		// --------------------------------------------------------------------------
		
		private function _(id:String):String {
			return LoadScreen.clientGetText._(id);
		}
	}
}