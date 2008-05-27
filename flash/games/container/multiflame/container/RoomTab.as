﻿package multiflame.container {	import flash.display.*;	import flash.events.*;	import flash.text.*;	import flash.utils.getQualifiedClassName;	import flash.utils.getDefinitionByName;	import gs.TweenLite;	import gs.TweenFilterLite;	import multiflame.game.IContainer;	import multiflame.game.IGame;	import multiflame.game.IConfigDlg;	import multiflame.game.Constants;		import multiflame.container.events.*;	public class RoomTab extends Sprite implements IContainer {		private var _channel:Channel;		private var _game:IGame;		private var _gameInfo:Object;		// Timeout calculator is disabled if this player joins at the middle of a game		private var _firstAction:Boolean;		private var _timeoutCalculator:TimeoutCalculator;		private var _actionSystemTime:Number;		private var _actionTimestamp:Number;		private var _moveSent:Boolean;  // Mark so that the default move will not be sent		private var _defaultMove:Object;		private var _gameResult:Array;		private var _extraGameResult:String;		private var _lastActionResult:int;		private var _over:Boolean;		// --------------------------------------------------------------------------		private static var _instance:RoomTab;		public static function get instance():RoomTab {			if (_instance == null) {				_instance = new RoomTab(new SingletonEnforcer());			}			return _instance;		}		public function RoomTab(singletonEnforcer:SingletonEnforcer):void {			_channel = Channel.instance;			_channel.addEventListener(CloseEvent.CLOSED, onClosed);			_channel.addEventListener(RoomEnterLeaveEvent.ENTER_ME, onRoomEnterMe);			_channel.addEventListener(NewEvent.CONF, onNewConfig);			_channel.addEventListener(NewEvent.JOIN, onNewJoin);			_channel.addEventListener(NewEvent.UNJOIN, onNewUnjoin);			_channel.addEventListener(NewEvent.TIMEOUT, onNewTimeout);			_channel.addEventListener(PlayEvent.MOVE, onPlayMove);			_channel.addEventListener(PlayEvent.RESIGN, onPlayResign);			_channel.addEventListener(PlayEvent.TIMEOUT, onPlayTimeout);			_channel.addEventListener(GameOverEvent.GAME_OVER, onGameOver);						addEventListener(Event.ADDED, onAdded);		}		// --------------------------------------------------------------------------		public function _(id:String):String {			return LoadScreen.gameGetText._(id);		}		public function get TweenLite():* {			return gs.TweenLite;		}		public function get TweenFilterLite():* {			return gs.TweenFilterLite;		}		// ---------------------------------------------------------------------------		public function get baseConfig():Object {			return _channel.baseConfig;		}		public function get extendedConfig():Object {			return _channel.extendedConfig;		}		public function get nicks0():Array {			return _channel.playNicks0;		}		public function get indexMe() {			if (_channel.state != Channel.PLAY) {				return -1;			}			return _channel.playNicks0.indexOf(_channel.nick);		}		public function set defaultMove(value:Object):void {			_defaultMove = value;		}		public function get defaultMove():Object {			return _defaultMove;		}		public function get gameResult():Array {			return _gameResult;		}		public function set extraGameResult(value:String):void {			_extraGameResult = value;		}		public function get lastActionResult():int {			return _lastActionResult;		}		public function onActionDone(result:int):void {			_lastActionResult = result;			_over = result == Constants.OVER;			if (_firstAction) {				_firstAction = false;				_timeoutCalculator = new TimeoutCalculator(_gameInfo.type, _channel.baseConfig, result);			}			_game.enabled = indexMe >= 0 &&				(result == Constants.ANY || (result >= 0 && indexMe == result));			if (_timeoutCalculator != null) {				var now:Number = (new Date()).time/1000;				var processingSec:Number = now - _actionSystemTime;				_timeoutCalculator.calc(_actionTimestamp, processingSec, result);				_channel.broadcastPlayActionResult(result,					_timeoutCalculator.moveSecLeft, _timeoutCalculator.totalSecLeft);			} else {				_channel.broadcastPlayActionResult(result, 0, 0);			}			if (_over && _channel.playNicks.indexOf(_channel.nick) >= 0) {				_channel.gameOver(_gameResult);			}		}		public function enqueueMove(data:Object):void {			_game.enabled = false;			_moveSent = true;			_channel.playMove([_game.snapshot, data]);		}		// --------------------------------------------------------------------------		public function get baseConfigRange():Object {			return null;//_baseConfigRange;		}		public function config(baseConfig:Object, extendedConfig:Object):void {			_channel.newConfig(baseConfig, extendedConfig);		}		public function join():void {			_channel.newJoin();		}		public function unjoin():void {			_channel.newUnjoin();		}		// --------------------------------------------------------------------------		public function set game(value:Sprite) {			_game = value as IGame;			_game.enabled = false;			_gameInfo = _game.onLoad(this);			if (_gameInfo.configDlg == null) {				_gameInfo.configDlg = new ConfigDlg();			}			_gameInfo.configDlg.onLoaded(this, _gameInfo);		}		public function get game():Sprite {			return _game as Sprite;		}		public function get gameInfo():Object {			return _gameInfo;		}		/**		 * @return		 * true if the default move was enqueued.		 */		public function enqueueDefaultMove():Boolean {			if (baseConfig.moveSec > 0 && !_moveSent && _defaultMove != null) {				enqueueMove(_defaultMove);				return true;			}			return false;		}		// --------------------------------------------------------------------------		private function onAdded(event:Event):void {			if (event.target != this) {				return;			}			if (!contains(_game as Sprite)) {				addChild(_game as Sprite);			}		}		// --------------------------------------------------------------------------		private function onRoomEnterMe(event:RoomEnterLeaveEvent):void {			switch (_channel.state) {			case Channel.NEWABLE:				_gameInfo.configDlg.onTimeout();				addChild(_gameInfo.configDlg as Sprite);				break;			case Channel.NEW:				// Playback				_gameInfo.configDlg.onConfig(nicks0[0], nicks0[0] == _channel.nick,					_channel.baseConfig, _channel.extendedConfig);				for (var i:int = 1; i < nicks0.length; i++) {					_gameInfo.configDlg.onJoin(nicks0[i]);				}				addChild(_gameInfo.configDlg as Sprite);				break;			case Channel.PLAY:				var playActions:Array = event.snapshot[6];				_game.enabled = false;				initGameResult();				_game.onNewGame(event.snapshot[5]);				TweenFilterLite.to(_game, 1.5, {});				break;			}		}		private function onClosed(event:CloseEvent):void {			if (event.type == CloseEvent.CLOSED) {							_game.enabled = false;				var dlg:Sprite  = _gameInfo.configDlg as Sprite;				if (contains(dlg)) {					removeChild(dlg);				}			}		}		// --------------------------------------------------------------------------		private function onNewConfig(event:NewEvent):void {			_gameInfo.configDlg.onConfig(event.nick, event.nick == _channel.nick,				_channel.baseConfig, _channel.extendedConfig);		}		private function onNewJoin(event:NewEvent):void {			if (_channel.state == Channel.PLAY) {				if (contains(_gameInfo.configDlg as Sprite)) {					removeChild(_gameInfo.configDlg as Sprite);				}				TweenFilterLite.to(_game, 1.5, {});				initGameResult();				_over = false;				_firstAction = true;				_actionSystemTime = (new Date()).time/1000;				_game.onNewGame(null);			} else {				_gameInfo.configDlg.onJoin(event.nick);			}		}		private function onNewUnjoin(event:NewEvent):void {			if (_channel.state == Channel.NEWABLE) {				_gameInfo.configDlg.onTimeout();			} else {				_gameInfo.configDlg.onUnjoin(event.nick);			}		}		private function onNewTimeout(event:NewEvent):void {			_gameInfo.configDlg.onTimeout();		}		// --------------------------------------------------------------------------		private function onPlayMove(event:PlayEvent):void {			if (_over) {				return;			}			if (_timeoutCalculator != null) {				_actionSystemTime = (new Date()).time/1000;				_actionTimestamp = event.timestamp;			}			_moveSent = false;			_game.onMove(event.timestamp, event.moves);		}		private function onPlayResign(event:PlayEvent):void {			if (_over) {				return;			}			_actionSystemTime = (new Date()).time/1000;			_actionTimestamp = event.timestamp;			_moveSent = false;			_game.onResign(event.timestamp, event.index);		}		// Not called if this player joined at the middle of this game.		private function onPlayTimeout(event:PlayEvent):void {			if (_over) {				return;			}			_actionSystemTime = (new Date()).time/1000;			_actionTimestamp = event.timestamp;			var timeOutResult = _timeoutCalculator.checkTimeout(event.timestamp, event.index);			_moveSent = false;			_game.onTimeout(event.timestamp, timeOutResult[0], timeOutResult[1]);		}		// --------------------------------------------------------------------------		private function onGameOver(event:GameOverEvent):void {			_game.enabled = false;			TweenFilterLite.to(_game, 1.5, {colorize: 0x000000, amount: 0.5});			_gameInfo.configDlg.onGameResult(_channel.playNicks0, _gameResult, _extraGameResult);			addChild(_gameInfo.configDlg as Sprite);		}		private function initGameResult():void {			_gameResult = new Array(baseConfig.nPlayers);			for (var i:int = 0; i < baseConfig.nPlayers; i++) {				_gameResult[i] = Constants.NONE;			}		}	}}class SingletonEnforcer {}