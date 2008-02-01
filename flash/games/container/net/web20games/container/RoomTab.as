package net.web20games.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import flash.utils.getQualifiedClassName;
	import flash.utils.getDefinitionByName;

	import gs.TweenLite;
	import gs.TweenFilterLite;

	import net.web20games.game.IContainer;
	import net.web20games.game.IDocument;
	import net.web20games.game.IConfigDlg;
	import net.web20games.game.Game;
	import net.web20games.container.events.*;

	public class RoomTab extends Sprite implements IContainer {
		// Play actions
		public static const MOVE:int    = 0;
		public static const RESIGN:int  = 1;
		public static const TIMEOUT:int = 2;

		private var _gameDocument:Sprite;
		private var _gameClass:Class;
		private var _gameInstance:Game;
		private var _definition:Object;
		private var _over:Boolean;

		private var _configDlg:IConfigDlg;
		private var _introSprite:Sprite;

		private var _channel:Channel;
		private var _timeoutCalculator:TimeoutCalculator;

		// --------------------------------------------------------------------------

		private static var _instance:RoomTab;

		public function RoomTab(singletonEnforcer:SingletonEnforcer):void {
			_channel = Channel.instance;

			_channel.addEventListener(CloseEvent.CLOSED, onClosed);
			_channel.addEventListener(RoomEnterLeaveEvent.ENTER_ME, onRoomEnterMe);
			_channel.addEventListener(NewEvent.INIT, onNewInit);
			_channel.addEventListener(NewEvent.JOIN, onNewJoin);
			_channel.addEventListener(NewEvent.UNJOIN, onNewUnjoin);
			_channel.addEventListener(NewEvent.TIMEOUT, onNewTimeout);
			_channel.addEventListener(PlayEvent.MOVE, onPlayMove);
			_channel.addEventListener(PlayEvent.RESIGN, onPlayResign);
			_channel.addEventListener(PlayEvent.TIMEOUT, onPlayTimeout);
			_channel.addEventListener(GameOverEvent.GAME_OVER, onGameOver);
			_channel.addEventListener(ResultEvent.RESULT, onResult);
			
			addEventListener(Event.ADDED, onAdded);
		}

		public static function get instance():RoomTab {
			if (_instance == null) {
				_instance = new RoomTab(new SingletonEnforcer());
			}
			return _instance;
		}

		// --------------------------------------------------------------------------

		public function get definition():Object {
			if (_definition == null) {
				_definition = _gameInstance.definition;
			}
			return _definition;
		}

		public function init(baseConfig:Object, extendedConfig:Object):void {
			_channel.newInit(baseConfig, extendedConfig);
		}

		public function join():void {
			_channel.newJoin();
		}

		public function unjoin():void {
			_channel.newUnjoin();
		}

		
		public function move(data:Object):void {
			_gameInstance.enabled = false;
			_channel.playMove(data);
		}

		public function _(id:String):String {
			return LoadScreen.gameGetText._(id);
		}

		// --------------------------------------------------------------------------

		public function set gameDocument(value:Sprite) {
			_gameDocument = value;

			var idoc:IDocument = value as IDocument;
			var game:Object = idoc.game;
			_gameClass = game.klass;
			_gameInstance = game.instance;
			_gameInstance.enabled = false;

			var o:Object = _gameInstance.onContainerSetWrapper(this, false);
			_configDlg = o.configDlg;
			if (_configDlg == null) {
				_configDlg = new ConfigDlg();
			}
			_configDlg.container = this;
			_introSprite = o.introSprite;
		}

		public function get gameDocument():Sprite {
			return _gameDocument;
		}

		public function get introSprite():Sprite {
			return _introSprite;
		}

		public function get TweenLite():* {
			return gs.TweenLite;
		}

		public function get TweenFilterLite():* {
			return gs.TweenFilterLite;
		}

		// --------------------------------------------------------------------------

		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}

			if (!contains(_gameInstance)) {
				addChild(_gameInstance);
				TweenFilterLite.to(_gameInstance, 1.5, {type: "Color"});
			}
		}

		// --------------------------------------------------------------------------

		private function onRoomEnterMe(event:RoomEnterLeaveEvent):void {
			switch (_channel.state) {
			case Channel.NEWABLE:
				_configDlg.onTimeout();
				addChild(_configDlg as Sprite);
				break;
			case Channel.NEW:
				playbackNewActions(_channel.baseConfig, _channel.extendedConfig,
					_channel.playNicks0);
				addChild(_configDlg as Sprite);
				break;
			case Channel.PLAY:
				var playActions:Array = event.snapshot[5];
				_gameInstance.enabled = false;
				var ret:int = _gameInstance.onNewGameWrapper(_channel.baseConfig, _channel.extendedConfig,
					_channel.playNicks0, -1, false);

				_timeoutCalculator = new TimeoutCalculator(_definition.klass, _channel.baseConfig, ret);
				playbackPlayActions(_gameInstance, playActions, _timeoutCalculator);

				_channel.broadcastPlayActionResult(ret,
					_timeoutCalculator.moveSecLeft, _timeoutCalculator.totalSecLeft);
				break;
			}
		}

		private function onClosed(event:CloseEvent):void {
			if (event.type == CloseEvent.CLOSED) {			
				_gameInstance.enabled = false;
				var dlg:Sprite  = _configDlg as Sprite;
				if (contains(dlg)) {
					removeChild(dlg);
				}
			}
		}

		// --------------------------------------------------------------------------

		private function onNewInit(event:NewEvent):void {
			_configDlg.onInit(event.nick, event.nick == _channel.nick,
				_channel.baseConfig, _channel.extendedConfig);
		}

		private function onNewJoin(event:NewEvent):void {
			if (_channel.state == Channel.PLAY) {
				if (contains(_configDlg as Sprite)) {
					removeChild(_configDlg as Sprite);
				}

				var indexMe:int = _channel.playNicks0.indexOf(_channel.nick);
				var ret:int = _gameInstance.onNewGameWrapper(_channel.baseConfig, _channel.extendedConfig,
					_channel.playNicks0, indexMe, false);
				_gameInstance.enabled = indexMe >= 0 && (ret == Game.A_ANY || indexMe == ret);
				_over = false;				

				TweenFilterLite.to(_gameInstance, 1.5, {type: "Color"});
				_timeoutCalculator = new TimeoutCalculator(_definition.klass, _channel.baseConfig, ret);

				_channel.broadcastPlayActionResult(ret,
					_timeoutCalculator.moveSecLeft, _timeoutCalculator.totalSecLeft);
			} else {
				_configDlg.onJoin(event.nick);
			}
		}

		private function onNewUnjoin(event:NewEvent):void {
			if (_channel.state == Channel.NEWABLE) {
				_configDlg.onTimeout();
			} else {
				_configDlg.onUnjoin(event.nick);
			}
		}

		private function onNewTimeout(event:NewEvent):void {
			_configDlg.onTimeout();
		}

		// --------------------------------------------------------------------------

		private function onPlayMove(event:PlayEvent):void {
			if (_over) {
				return;
			}

			var ret:int = _gameInstance.onMoveWrapper(event.timestamp, event.moves, false);
			_over = ret == Game.A_OVER;

			var indexMe:int = _channel.playNicks0.indexOf(_channel.nick);
			_gameInstance.enabled = indexMe >= 0 &&
				(ret == Game.A_ANY || (ret >= 0 && indexMe == ret));

			_timeoutCalculator.onMove(event.timestamp, ret);
			_channel.broadcastPlayActionResult(ret,
				_timeoutCalculator.moveSecLeft, _timeoutCalculator.totalSecLeft);

			if (_over && _channel.playNicks.indexOf(_channel.nick) >= 0) {
				_channel.gameOver();
			}
		}

		private function onPlayResign(event:PlayEvent):void {
			if (_over) {
				return;
			}

			var ret:int = _gameInstance.onResignWrapper(event.timestamp, event.index, false);
			_over = ret == Game.A_OVER;

			var indexMe:int = _channel.playNicks0.indexOf(_channel.nick);
			_gameInstance.enabled = indexMe >= 0 &&
				(ret == Game.A_ANY || (ret >= 0 && indexMe == ret));

			_timeoutCalculator.onResign(event.timestamp, ret);
			_channel.broadcastPlayActionResult(ret,
				_timeoutCalculator.moveSecLeft, _timeoutCalculator.totalSecLeft);

			if (_over && _channel.playNicks.indexOf(_channel.nick) >= 0) {
				_channel.gameOver();
			}
		}

		private function onPlayTimeout(event:PlayEvent):void {
			if (_over) {
				return;
			}

			var timeOutResult = _timeoutCalculator.checkTimeout(event.timestamp, event.index);
			var ret:int = _gameInstance.onTimeoutWrapper(event.timestamp, timeOutResult[0], timeOutResult[1], false);
			_over = ret == Game.A_OVER;

			var indexMe:int = _channel.playNicks0.indexOf(_channel.nick);
			_gameInstance.enabled = indexMe >= 0 &&
				(ret == Game.A_ANY || (ret >= 0 && indexMe == ret));

			_timeoutCalculator.onTimeout(event.timestamp, ret);
			_channel.broadcastPlayActionResult(ret,
				_timeoutCalculator.moveSecLeft, _timeoutCalculator.totalSecLeft);

			if (_over && _channel.playNicks.indexOf(_channel.nick) >= 0) {
				_channel.gameOver();
			}
		}

		// --------------------------------------------------------------------------

		private function onGameOver(event:GameOverEvent):void {
			_gameInstance.enabled = false;
			TweenFilterLite.to(_gameInstance, 1.5, {type: "Color", colorize: 0x000000, amount: 0.5});
		}

		// --------------------------------------------------------------------------

		public function onJudge(baseConfig:Object, extendedConfig:Object,
				playActions:Array, indexReporter:int):Array {
			var i:int;

			var tmpNicks:Array = new Array(baseConfig.nPlayers);
			for (i = 0; i < baseConfig.nPlayers; i++) {
				tmpNicks[i] = "" + i;
			}

			var tmpGameInstance:Game = new _gameClass();
			tmpGameInstance.onContainerSetWrapper(this, true);
			var newGameActionResult:int = tmpGameInstance.onNewGameWrapper(
				baseConfig, extendedConfig, tmpNicks, 0, true);
			var tmpTimeoutCalculator:TimeoutCalculator = new TimeoutCalculator(
				_definition.klass, baseConfig, newGameActionResult);
			playbackPlayActions(tmpGameInstance, playActions, tmpTimeoutCalculator);
			
			var ret:Array;
			if (tmpGameInstance.lastActionResult == Game.A_OVER) {
				ret = tmpGameInstance.gameResult;
			} else {
				// This game is not over, we must forcefully compute the result
				ret = tmpGameInstance.gameResult;
				ret[indexReporter] = Game.P_LOST;
				for (i = 0; i < baseConfig.nPlayers; i++) {
					if (ret[i] == Game.P_NONE) {
						ret[i] = Game.P_DREW;
					}
				}
			}
			tmpTimeoutCalculator = null;
			tmpGameInstance = null;

			return ret;
		}

		private function onResult(event:ResultEvent):void {
			_configDlg.onResult(_channel.playNicks0, event.result, _gameInstance.summary);
			addChild(_configDlg as Sprite);
		}

		// --------------------------------------------------------------------------

		// Not actually playback because only onNewInit() and onNewJoin() are called.
		private function playbackNewActions(baseConfig:Object, extendedConfig:Object,
				playNicks0:Array):void {
			_configDlg.onInit(playNicks0[0], playNicks0[0] == _channel.nick,
				baseConfig, extendedConfig);
			for (var i:int = 1; i < playNicks0.length; i++) {
				_configDlg.onJoin(playNicks0[i]);
			}
		}

		private function playbackPlayActions(gameInstance:Game, playActions:Array,
				timeoutCalculator:TimeoutCalculator) {
			var action:Array;
			var timestamp:int;
			var index:int;
			var moves:Array;
			var type:int;

			var timeOutResult:Array;
			var ret:int;

			for (var i:int = 0; i < playActions.length; i++) {
				action = playActions[i];
				timestamp = action.shift();
				if (action.length == 1) {
					if (action[0] < 0) {
						type = TIMEOUT;
						index = -(action[0] + 1);
					} else {
						type = RESIGN;
						index = action[0];
					}
				} else {
					type = MOVE;
					moves = action;
				}

				switch (type) {
				case MOVE:
					ret = gameInstance.onMoveWrapper(timestamp, moves, true);
					if (ret == Game.A_OVER) {
						return;
					}
					timeoutCalculator.onMove(timestamp, ret);
					break;
				case RESIGN:
					ret = gameInstance.onResignWrapper(timestamp, index, true);
					if (ret == Game.A_OVER) {
						return;
					}
					timeoutCalculator.onResign(timestamp, ret);
					break;
				case TIMEOUT:
					timeOutResult = timeoutCalculator.checkTimeout(timestamp, index);
					ret = gameInstance.onTimeoutWrapper(timestamp, timeOutResult[0], timeOutResult[1], true);
					if (ret == Game.A_OVER) {
						return;
					}
					timeoutCalculator.onTimeout(timestamp, ret);
					break;
				}
			}
		}
	}
}

class SingletonEnforcer {}