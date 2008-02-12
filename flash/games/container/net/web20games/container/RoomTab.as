package net.web20games.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import flash.utils.getQualifiedClassName;
	import flash.utils.getDefinitionByName;

	import gs.TweenLite;
	import gs.TweenFilterLite;

	import net.web20games.game.IContainer;
	import net.web20games.game.IGame;
	import net.web20games.game.IConfigDlg;
	import net.web20games.game.Constants;	

	import net.web20games.container.events.*;

	public class RoomTab extends Sprite implements IContainer {
		private var _enabled:Boolean;

		private var _channel:Channel;

		private var _game:IGame;
		private var _introSprite:Sprite;

		private var _configDlg:IConfigDlg;

		private var _timeoutCalculator:TimeoutCalculator;
		private var _actionSystemTime:Number;
		private var _actionTimestamp:Number;

		private var _gameResult:Array;
		private var _lastActionResult:int;
		private var _over:Boolean;
		private var _extra:String;

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
			
			addEventListener(Event.ADDED, onAdded);
		}

		public static function get instance():RoomTab {
			if (_instance == null) {
				_instance = new RoomTab(new SingletonEnforcer());
			}
			return _instance;
		}

		// --------------------------------------------------------------------------

		public function _(id:String):String {
			return LoadScreen.gameGetText._(id);
		}

		public function get TweenLite():* {
			return gs.TweenLite;
		}

		public function get TweenFilterLite():* {
			return gs.TweenFilterLite;
		}

		// ---------------------------------------------------------------------------

		public function get enabled():Boolean {
			return _enabled;
		}

		public function get baseConfig():Object {
			return _channel.baseConfig;
		}

		public function get extendedConfig():Object {
			return _channel.extendedConfig;
		}

		public function get nicks0():Array {
			return _channel.playNicks0;
		}

		public function get indexMe() {
			return _channel.playNicks0.indexOf(_channel.nick);
		}

		public function get gameResult():Array {
			return _gameResult;
		}

		public function get lastActionResult():int {
			return _lastActionResult;
		}

		public function setActionResult(result:int, extra:String = null):void {
			_lastActionResult = result;
			_over = result == Constants.OVER;
			_extra = extra;

			_enabled = indexMe >= 0 &&
				(result == Constants.ANY || (result >= 0 && indexMe == result));

			var now:Number = (new Date()).time/1000;
			var processingSec:Number = now - _actionSystemTime;
			_timeoutCalculator.calc(_actionTimestamp, processingSec, result);
			_channel.broadcastPlayActionResult(result,
				_timeoutCalculator.moveSecLeft, _timeoutCalculator.totalSecLeft);

			if (_over && _channel.playNicks.indexOf(_channel.nick) >= 0) {
				_channel.gameOver();
			}
		}

		public function enqueueMove(data:Object):void {
			_enabled = false;
			_channel.playMove(data);
		}

		// --------------------------------------------------------------------------

		public function get definition():Object {
			return _game.definition;
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

		// --------------------------------------------------------------------------

		public function set game(value:Sprite) {
			_game = value as IGame;
			_enabled = false;

			var o:Object = _game.setContainer(this);
			_configDlg = o.configDlg;
			if (_configDlg == null) {
				_configDlg = new ConfigDlg();
			}
			_configDlg.setContainer(this);
			_introSprite = o.introSprite;
		}

		public function get game():Sprite {
			return _game as Sprite;
		}

		public function get introSprite():Sprite {
			return _introSprite;
		}

		// --------------------------------------------------------------------------

		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}

			if (!contains(_game as Sprite)) {
				addChild(_game as Sprite);
				TweenFilterLite.to(_game, 1.5, {type: "Color"});
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
				// Playback
				_configDlg.onInit(nicks0[0], nicks0[0] == _channel.nick,
					_channel.baseConfig, _channel.extendedConfig);
				for (var i:int = 1; i < nicks0.length; i++) {
					_configDlg.onJoin(nicks0[i]);
				}

				addChild(_configDlg as Sprite);
				break;
			case Channel.PLAY:
				_enabled = false;
				initGameResult();
				var snapshot:Object = event.snapshot[5];
				_lastActionResult = _game.onNewGame(snapshot);

				_timeoutCalculator = new TimeoutCalculator(_game.definition.klass, _channel.baseConfig, _lastActionResult);
				_channel.broadcastPlayActionResult(_lastActionResult,
					_timeoutCalculator.moveSecLeft, _timeoutCalculator.totalSecLeft);
				break;
			}
		}

		private function onClosed(event:CloseEvent):void {
			if (event.type == CloseEvent.CLOSED) {			
				_enabled = false;
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

				initGameResult();
				_lastActionResult = _game.onNewGame(null);
				_enabled = indexMe >= 0 && (_lastActionResult == Constants.ANY || indexMe == _lastActionResult);
				_over = false;				

				TweenFilterLite.to(_game, 1.5, {type: "Color"});

				_timeoutCalculator = new TimeoutCalculator(_game.definition.klass, _channel.baseConfig, _lastActionResult);
				_channel.broadcastPlayActionResult(_lastActionResult,
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
			_actionSystemTime = (new Date()).time/1000;
			_actionTimestamp = event.timestamp;
			_game.onMove(event.timestamp, event.moves);
		}

		private function onPlayResign(event:PlayEvent):void {
			if (_over) {
				return;
			}
			_actionSystemTime = (new Date()).time/1000;
			_actionTimestamp = event.timestamp;
			_game.onResign(event.timestamp, event.index);
		}

		private function onPlayTimeout(event:PlayEvent):void {
			if (_over) {
				return;
			}
			_actionSystemTime = (new Date()).time/1000;
			_actionTimestamp = event.timestamp;
			var timeOutResult = _timeoutCalculator.checkTimeout(event.timestamp, event.index);
			_game.onTimeout(event.timestamp, timeOutResult[0], timeOutResult[1]);
		}

		// --------------------------------------------------------------------------

		private function onGameOver(event:GameOverEvent):void {
			_enabled = false;
			TweenFilterLite.to(_game, 1.5, {type: "Color", colorize: 0x000000, amount: 0.5});
			_configDlg.onResult(_channel.playNicks0, _gameResult, _extra);
			addChild(_configDlg as Sprite);
		}

		private function initGameResult():void {
			_gameResult = new Array(baseConfig.nPlayers);
			for (var i:int = 0; i < baseConfig.nPlayers; i++) {
				_gameResult[i] = Constants.NONE;
			}
		}
	}
}

class SingletonEnforcer {}