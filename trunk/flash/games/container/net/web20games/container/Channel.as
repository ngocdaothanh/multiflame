package net.web20games.container {
	import flash.events.EventDispatcher;
	import flash.utils.ByteArray;

	import net.web20games.container.events.*;
	import net.web20games.utils.*;

	/**
	 * onXXX: The server will call these methods.
	 * Other methods: The client uses these methods to enqueue messages to the server.
	 *
	 * The instance of this class acts as the local broadcaster for the whole client.
	 * Information that needs to be shared among objects of many classes is posted
	 * here to be broadcasted.
	 */
	public class Channel extends EventDispatcher {
		public static const WEB_HOST:String = "web20games.net";

		public static const DEFAULT_FIFO_HOST:String = "web20games.net";
		public static const DEFAULT_FIFO_PORT:int    = 443;

		// Commands to send to the server
		public static const CMD_CAPTCHA:String      = '0'
		public static const CMD_GAME_INFO:int       = 1;
		public static const CMD_LOGIN:int           = 2;
		public static const CMD_WILL_CLOSE:int      = 3;
		public static const CMD_LOGOUT:int          = 4;
		public static const CMD_ROOM_ENTER:int      = 5;
		public static const CMD_ROOM_LEAVE:int      = 6;
		public static const CMD_CHAT:int            = 7;
		public static const CMD_NEW_INIT:int        = 8;
		public static const CMD_NEW_JOIN:int        = 9;
		public static const CMD_NEW_UNJOIN:int      = 10;
		public static const CMD_NEW_TIMEOUT:int     = 11;
		public static const CMD_PLAY_MOVE:int       = 12;
		public static const CMD_PLAY_RESIGN:int     = 13;
		public static const CMD_PLAY_TIMEOUT:int    = 14;
		public static const CMD_GAME_OVER:int       = 15;
		public static const CMD_JUDGE:int           = 16;
		public static const CMD_RESULT:int          = 17;

		// States
		public static const NOT_GOT_GAME_INFO:int = -3;
		public static const NOT_LOGGED_IN:int     = -2;
		public static const IN_LOBBY:int          = -1;
		public static const NEWABLE:int           = 0;
		public static const NEW:int               = 1;
		public static const PLAY:int              = 2;
		public static const DRAW:int              = 3;
		public static const GAME_OVER:int         = 4;

		public static const NEW_TIMEOUT:int = 30;  // [s]

		// Although the following variable can be extracted from events, they are put
		// here for convenience -----------------------------------------------------

		public var state:int;

		// Local game info
		public var host:String;
		public var id:int;
		public var channel:String;
		public var locale:String;
		public var containerVersion:int;
		public var gameVersion:int;
		
		// Remote game info
		public var gameRemoteInfo:Object;

		// Toy
		public var toyId:int;
		public var toyConfig:XML;
		public var toy:*;

		// Initialized onLoginMe
		public var nick:String;  // Appended with GUEST_SUFFIX if this is a guest player
		public var nicks:Array;
		public var baseConfig:Object;
		public var extendedConfig:Object;
		public var playNicks0:Array;
		public var playNicks:Array;
		
		// --------------------------------------------------------------------------

		private static var _instance:Channel;
		private var _transporter:Transporter;

		// AS does not allow private constructor.
		public function Channel(singletonEnforcer:SingletonEnforcer):void {
			state = NOT_GOT_GAME_INFO;
		}

		public static function get instance():Channel {
			if (_instance == null) {
				_instance = new Channel(new SingletonEnforcer());
			}
			return _instance;
		}
		
		// --------------------------------------------------------------------------
	
		public function broadcastGameInfoLocal(host:String, id:int, channel:String, locale:String,
				 containerVersion:int, gameVersion:int):void {
			this.host             = host;
			this.id               = id;
			this.channel          = channel;
			this.locale           = locale;
			this.containerVersion = containerVersion;
			this.gameVersion      = gameVersion;

			_transporter = new Transporter(host, 443);

			_transporter.addEventListener("" + TransporterEvent.CONNECTION_ERROR, onConnectionError);
			_transporter.addEventListener("" + TransporterEvent.CONNECTION_CLOSE, onConnectionClose);

			_transporter.addEventListener("" + CMD_GAME_INFO, onGameInfo);
			_transporter.addEventListener("" + CMD_LOGIN, onLogin);
			_transporter.addEventListener("" + CMD_WILL_CLOSE, onWillClose);
			_transporter.addEventListener("" + CMD_LOGOUT, onLogout);
			_transporter.addEventListener("" + CMD_ROOM_ENTER, onRoomEnter);
			_transporter.addEventListener("" + CMD_ROOM_LEAVE, onRoomLeave);
			_transporter.addEventListener("" + CMD_CHAT, onChat);
			_transporter.addEventListener("" + CMD_NEW_INIT, onNewInit);
			_transporter.addEventListener("" + CMD_NEW_JOIN, onNewJoin);
			_transporter.addEventListener("" + CMD_NEW_UNJOIN, onNewUnjoin);
			_transporter.addEventListener("" + CMD_NEW_TIMEOUT, onNewTimeout);
			_transporter.addEventListener("" + CMD_PLAY_MOVE, onPlayMove);
			_transporter.addEventListener("" + CMD_PLAY_RESIGN, onPlayResign);
			_transporter.addEventListener("" + CMD_PLAY_TIMEOUT, onPlayTimeout);
			_transporter.addEventListener("" + CMD_GAME_OVER, onGameOver);
			_transporter.addEventListener("" + CMD_JUDGE, onJudge);
			_transporter.addEventListener("" + CMD_RESULT, onResult);

			var e:GameInfoEvent = new GameInfoEvent(GameInfoEvent.LOCAL);
			e.id        = id;
			e.channel   = channel;
			e.locale    = locale;
			dispatchEvent(e);
		}
		
		public function broadcastPlayActionResult(actionResult:int, moveSecLeft:int, totalSecLeft:int):void {
			var e:PlayEvent = new PlayEvent(PlayEvent.ACTION_RESULT);
			e.actionResult = actionResult;
			e.moveSecLeft = moveSecLeft;
			e.totalSecLeft = totalSecLeft;
			dispatchEvent(e);
		}

		// --------------------------------------------------------------------------

		private function onConnectionError(event:TransporterEvent):void {
			if (state == NOT_GOT_GAME_INFO) {
				var ge:GameInfoEvent = new GameInfoEvent(GameInfoEvent.REMOTE);
				ge.code = GameInfoEvent.CONNECTION_ERROR;
				dispatchEvent(ge);
			} else {
				var le:LoginoutEvent = new LoginoutEvent(LoginoutEvent.LOGIN_ME, null);
				le.code = LoginoutEvent.CONNECTION_ERROR;
				dispatchEvent(le);
			}
		}

		private function onConnectionClose(event:TransporterEvent):void {
			if (state > NOT_LOGGED_IN) {
				state = NOT_LOGGED_IN;
				dispatchEvent(new CloseEvent(CloseEvent.CLOSED));
			}
		}

		// --------------------------------------------------------------------------

		public function getGameInfo():void {
			_transporter.invoke(CMD_GAME_INFO, [id, locale]);
		}

		private function onGameInfo(event:TransporterEvent):void {
			var a:Array = event.arg as Array;
			var code:int = a[0];
			var info:Object;
			if (code == GameInfoEvent.OK) {
				info = a[1];
				state = NOT_LOGGED_IN;
				gameRemoteInfo = info;
			}
			var e:GameInfoEvent = new GameInfoEvent(GameInfoEvent.REMOTE);
			e.code = code;
			e.info = info;
			dispatchEvent(e);
		}
		
		// --------------------------------------------------------------------------

		public function login(code:String, encryptedCode:String, nick:String):void {
			_transporter.invoke(CMD_LOGIN, [
				containerVersion,
				id,
				gameVersion,
				channel,
				code,
				encryptedCode,
				nick,
				RoomTab.instance.definition.klass
			]);
			this.nick = nick;
		}

		private function onLogin(event:TransporterEvent):void {
			if (state == NOT_LOGGED_IN) {
				onLoginMe(event);
			} else {
				var nick:String = event.arg as String;
				nicks.push(nick);
				dispatchEvent(new LoginoutEvent(LoginoutEvent.LOGIN, nick));
			}
		}

		// [[nicks in lobby], [nicks in room0], [nicks in room1]...]
		private function onLoginMe(event:TransporterEvent):void {
			var a:Array = event.arg as Array;
			var code:int       = a[0];
			var snapshot:Array = a[1];

			if (code == LoginoutEvent.OK) {
				state = IN_LOBBY;
				nicks = snapshot[0];
			}

			var e:LoginoutEvent = new LoginoutEvent(LoginoutEvent.LOGIN_ME, nick);
			e.code = code;
			e.snapshot = snapshot;
			dispatchEvent(e);
		}

		// --------------------------------------------------------------------------

		private function onLogout(event:TransporterEvent):void {
			var nick:String = event.arg as String;
			nicks.splice(nicks.indexOf(nick), 1);
			dispatchEvent(new LoginoutEvent(LoginoutEvent.LOGOUT, nick));
		}

		// --------------------------------------------------------------------------

		public function room_enter(iroom:int):void {
			_transporter.invoke(CMD_ROOM_ENTER, iroom);
		}

		// arg:
		// * For the players who are in the room:  nick
		// * For the player who entered the room:  room snapshot
		// * For the players who are in the lobby: [iroom, nick]
		private function onRoomEnter(event:TransporterEvent):void {
			var a:Array;
			var e:RoomEnterLeaveEvent;

			if (state == IN_LOBBY) {
				a = event.arg as Array;
				if (a.length > 2) {
					// Case 2
					onRoomEnterMe(event);
				} else {
					// Case 3
					e = new RoomEnterLeaveEvent(RoomEnterLeaveEvent.ENTER_OTHER_ME_LOBBY);
					e.iroom = a[0];
					e.nick = a[1];
					nicks.splice(nicks.indexOf(e.nick), 1);
					dispatchEvent(e);
				}
			} else {
				// Case 1
				e = new RoomEnterLeaveEvent(RoomEnterLeaveEvent.ENTER_OTHER_ME_ROOM);
				e.nick = event.arg as String;
				nicks.push(e.nick);
				dispatchEvent(e);
			}
		}

		// [state, nicks, baseConfig, extendedConfig, playNicks0, moves]
		private function onRoomEnterMe(event:TransporterEvent):void {
			var snapshot:Array = event.arg as Array;

			state = snapshot[0];
			nicks = snapshot[1];
			if (snapshot[2] != null) {
				snapshot[2] = {
					nPlayers: snapshot[2][0],
					moveSec:  snapshot[2][1],
					totalMin: snapshot[2][2]
				};
			} else {
				var definition = RoomTab.instance.definition;
				snapshot[2] = {
					nPlayers: definition.nPlayersMin,
					moveSec:  definition.moveSecMin,
					totalMin: definition.totalMinMin
				};
			}
			baseConfig     = snapshot[2];
			extendedConfig = snapshot[3];
			playNicks0     = snapshot[4]
			if (state == PLAY) {
				playNicks = new Array();
				for (var i:int = 0; i < playNicks0.length; i++) {
					playNicks.push(playNicks0[i]);
				}
			}

			var e:RoomEnterLeaveEvent = new RoomEnterLeaveEvent(RoomEnterLeaveEvent.ENTER_ME);
			e.snapshot = snapshot;
			dispatchEvent(e);
		}

		public function roomLeave():void {
			_transporter.invoke(CMD_ROOM_LEAVE, null);
		}

		// arg:
		// * For the players who are in the room:  index
		// * For the players who are in the lobby: [iroom, nick]
		// * For the player who left:              room snapshot
		private function onRoomLeave(event:TransporterEvent):void {
			var a:Array;
			var e:RoomEnterLeaveEvent;

			if (event.arg is String) {
				// Case 1
				e = new RoomEnterLeaveEvent(RoomEnterLeaveEvent.LEAVE_OTHER_ME_ROOM);
				e.nick = event.arg as String;
				nicks.splice(nicks.indexOf(e.nick), 1);
				dispatchEvent(e);
			} else {
				a = event.arg as Array;
				if (a[0] is Array) {
					// Case 3
					onRoomLeaveMe(event);
				} else {
					// Case 2
					e = new RoomEnterLeaveEvent(RoomEnterLeaveEvent.LEAVE_OTHER_ME_LOBBY);
					e.iroom = a[0];
					e.nick = a[1];
					nicks.push(e.nick);
					dispatchEvent(e);
				}
			}
		}

		// Same as onLoginMe
		private function onRoomLeaveMe(event:TransporterEvent):void {
			var snapshot:Array = event.arg as Array;

			state = IN_LOBBY;
			nicks = snapshot[0];

			var e:RoomEnterLeaveEvent = new RoomEnterLeaveEvent(RoomEnterLeaveEvent.LEAVE_ME);
			e.snapshot = snapshot;
			dispatchEvent(e);
		}

		// --------------------------------------------------------------------------

		public function chat(msg:String):void {
			_transporter.invoke(CMD_CHAT, msg);
		}

		public function onChat(event:TransporterEvent):void {
			var a:Array = event.arg as Array;
			var index:int  = a[0];
			var msg:String = a[1];
			dispatchEvent(new ChatEvent(nicks[index], msg));
		}

		// --------------------------------------------------------------------------

		public function newInit(baseConfig:Object, extendedConfig:Object):void {
			var aBaseConfig:Array = [baseConfig.nPlayers, baseConfig.moveSec, baseConfig.totalMin];
			_transporter.invoke(CMD_NEW_INIT, [aBaseConfig, extendedConfig]);
		}

		private function onNewInit(event:TransporterEvent):void {
			var a:Array = event.arg as Array;
			var nick:String           = a[0];
			var aBaseConfig:Array     = a[1];
			var extendedConfig:Object = a[2];
			
			state = NEW;
			this.baseConfig = {
				nPlayers: aBaseConfig[0],
				moveSec:  aBaseConfig[1],
				totalMin: aBaseConfig[2]
			};
			this.extendedConfig = extendedConfig;
			playNicks0 = new Array(nick);
			
			var e:NewEvent = new NewEvent(NewEvent.INIT);
			e.nick           = nick;
			e.baseConfig     = baseConfig;
			e.extendedConfig = extendedConfig;
			dispatchEvent(e);
		}

		public function newJoin():void {
			_transporter.invoke(CMD_NEW_JOIN, null);
		}

		private function onNewJoin(event:TransporterEvent):void {
			var nick:String = event.arg as String;
			playNicks0.push(nick);
			if (playNicks0.length == baseConfig.nPlayers) {
				state = PLAY;
				playNicks = new Array();
				for (var i:int = 0; i < playNicks0.length; i++) {
					playNicks.push(playNicks0[i]);
				}
			}

			var e:NewEvent = new NewEvent(NewEvent.JOIN);
			e.nick = nick;
			dispatchEvent(e);
		}

		public function newUnjoin():void {
			_transporter.invoke(CMD_NEW_UNJOIN, null);
		}

		private function onNewUnjoin(event:TransporterEvent):void {
			var nick:String = event.arg as String;
			playNicks0.splice(playNicks0.indexOf(nick), 1);
			if (playNicks0.length == 0) {
				state = NEWABLE;
			}

			var e:NewEvent = new NewEvent(NewEvent.UNJOIN);
			e.nick = nick;
			dispatchEvent(e);
		}

		public function newTimeout():void {
			_transporter.invoke(CMD_NEW_TIMEOUT, null);
		}

		private function onNewTimeout(event:TransporterEvent):void {
			state = NEWABLE;
			dispatchEvent(new NewEvent(NewEvent.TIMEOUT));
		}

		// --------------------------------------------------------------------------

		public function playMove(data:Object):void {
			_transporter.invoke(CMD_PLAY_MOVE, data);
		}

		private function onPlayMove(event:TransporterEvent):void {
			var action:Array = event.arg as Array;
			var e:PlayEvent = new PlayEvent(PlayEvent.MOVE);
			e.timestamp = action.shift();
			e.moves = action;
			dispatchEvent(e);
		}

		public function playResign():void {
			_transporter.invoke(CMD_PLAY_RESIGN, null);
		}
		
		private function onPlayResign(event:TransporterEvent):void {
			var action:Array = event.arg as Array;
			var e:PlayEvent = new PlayEvent(PlayEvent.RESIGN);
			e.timestamp = action[0];
			e.index = action[1];

			var nick:String = playNicks0[e.index];
			playNicks.splice(playNicks.indexOf(nick), 1);

			dispatchEvent(e);
		}

		public function playTimeout():void {
			_transporter.invoke(CMD_PLAY_TIMEOUT, null);
		}
		
		private function onPlayTimeout(event:TransporterEvent):void {
			var action:Array = event.arg as Array;
			var e:PlayEvent = new PlayEvent(PlayEvent.TIMEOUT);
			e.timestamp = action[0];
			e.index = action[1];
			dispatchEvent(e);
		}

		// --------------------------------------------------------------------------
	
		public function gameOver():void {
			_transporter.invoke(CMD_GAME_OVER, null);
		}

		private function onGameOver(event:TransporterEvent):void {
			state = GAME_OVER;
			var e:GameOverEvent = new GameOverEvent();
			dispatchEvent(e);
		}

		// --------------------------------------------------------------------------

		// This is the only method where event is not used because the server wants
		// the judge result immediately.
		private function onJudge(event:TransporterEvent):void {
			var a:Array = event.arg as Array;
			var baseConfig:Object = {
				nPlayers: a[0][0],
				moveSec:  a[0][1],
				totalMin: a[0][2]
			};
			var extendedConfig:Object = a[1];
			var playActions:Array     = a[2];
			var indexReporter:int     = a[3]
			var result = RoomTab.instance.onJudge(baseConfig, extendedConfig,
				playActions, indexReporter);
			_transporter.invoke(CMD_RESULT, result);
		}

		private function onResult(event:TransporterEvent):void {
			var e:ResultEvent = new ResultEvent(event.arg as Array);
			dispatchEvent(e);
		}

		private function onWillClose(event:TransporterEvent):void {
			dispatchEvent(new CloseEvent(CloseEvent.WILL_CLOSE));
		}
	}
}

class SingletonEnforcer {}