package multiflame.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import fl.controls.*;

	import com.adobe.webapis.gettext.GetText;

	import multiflame.container.events.*;
	import multiflame.game.IConfigDlg;
	import multiflame.game.IContainer;

	public class ConfigDlg extends Sprite implements IConfigDlg {
		private var _container:IContainer;
		private var _nicks:Array;
		private var _nPlayersAlwaysDisabled:Boolean;
		private var _moveSecAlwaysDisabled:Boolean;
		private var _totalMinAlwaysDisabled:Boolean;
		
		public function ConfigDlg():void {
			addEventListener(Event.ADDED, onAdded);
			
			// Draging
			addEventListener(MouseEvent.MOUSE_DOWN, onMouseDown);
			addEventListener(MouseEvent.MOUSE_UP, onMouseUp);
			
			// Change mouse pointer
			_nPlayersLbl.selectable = false;
			_moveSecLbl.selectable  = false;
			_totalMinLbl.selectable = false;
			
			_joinCheckBox.addEventListener(MouseEvent.CLICK, onJoinClick);
			_newBtn.addEventListener(MouseEvent.CLICK, onNewBtnClick);
		}
		
		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}

			_moveSecLbl.text    = _("sec/move");
			_totalMinLbl.text   = _("minutes total");
			_joinCheckBox.label = _("Join");
			_newBtn.label       = _("New game");
		}
		
		// --------------------------------------------------------------------------
		
		public function setContainer(value:IContainer):void {
			_container = value;
			var range:Object = _container.baseConfigRange;

			_nPlayersStepper.minimum = range.nPlayersMin;
			_nPlayersStepper.maximum = range.nPlayersMax;
			_nPlayersStepper.value = _nPlayersStepper.minimum;
			_nPlayersAlwaysDisabled = _nPlayersStepper.minimum == _nPlayersStepper.maximum;
			if (_nPlayersAlwaysDisabled) {
				_nPlayersStepper.enabled = false;
			}
			
			_moveSecStepper.minimum = range.moveSecMin;
			_moveSecStepper.maximum = range.moveSecMax;
			_moveSecStepper.value = _moveSecStepper.minimum;
			_moveSecAlwaysDisabled = _moveSecStepper.minimum == _moveSecStepper.maximum;
			if (_moveSecAlwaysDisabled) {
				_moveSecStepper.enabled = false;
			}
			
			_totalMinStepper.minimum = range.totalMinMin;
			_totalMinStepper.maximum = range.totalMinMax;
			_totalMinStepper.value = _totalMinStepper.minimum;
			_totalMinAlwaysDisabled = _totalMinStepper.minimum == _totalMinStepper.maximum
			if (_totalMinAlwaysDisabled) {
				_totalMinStepper.enabled = false;
			}
		}

		public function onConfig(nick:String, me:Boolean, baseConfig:Object,
				extendedConfig:Object):void {
			_nicks = new Array(nick);
			_playerList.htmlText = "<p>" + nick + "</p>";

			_nPlayersStepper.value = baseConfig.nPlayers;
			_nPlayersStepper.enabled = false;

			_moveSecStepper.value = baseConfig.moveSec;
			_moveSecStepper.enabled = false;
			
			_totalMinStepper.value = baseConfig.totalMin;
			_totalMinStepper.enabled = false;
			
			// extendedConfig is ignored
			
			_newBtn.enabled = false;
			_joinCheckBox.selected = me;
			_joinCheckBox.enabled = true;
		}

		public function onJoin(nick:String):void {
			_joinCheckBox.enabled = true;
			_nicks.push(nick);
			_playerList.htmlText += "<p>" + nick + "</p>";
		}

		public function onUnjoin(nick:String):void {
			_nicks.splice(_nicks.indexOf(nick), 1);
			_playerList.htmlText = "";
			for (var i:int = 0; i < _nicks.length; i++) {
				_playerList.htmlText += "<p>" + _nicks[i] + "</p>";
			}
		}

		public function onTimeout():void {
			_nicks = new Array();
			_playerList.htmlText = "";

			if (!_nPlayersAlwaysDisabled) {
				_nPlayersStepper.enabled = true;
			}
			if (!_moveSecAlwaysDisabled) {
				_moveSecStepper.enabled = true;
			}
			if (!_totalMinAlwaysDisabled) {
				_totalMinStepper.enabled = true;
			}
		
			_newBtn.enabled = true;
			_joinCheckBox.selected = false;
			_joinCheckBox.enabled = false;
		}

		public function onGameResult(playNicks0:Array, result:Array, extra:String):void {
			onTimeout();

			var s:String = "";
			for (var i:int = 0; i < playNicks0.length; i++) {
				var ret:String;
				switch (result[i]) {
				case 0:
					ret = _("lost");
					break;
				case 1:
					ret = _("draw");
					break;
				case 3:
					ret = _("won");
					break;
				}
				s += "<p>" + playNicks0[i] + " " + ret + "</p>";
			}
			if (extra != null) {
				s += "<p>" + extra + "</p>";
			}
			_playerList.htmlText = s;
		}
		
		// --------------------------------------------------------------------------

		private function onJoinClick(event:MouseEvent):void {
			_joinCheckBox.enabled = false;
			if (_joinCheckBox.selected) {
				_container.join();
			} else {
				_container.unjoin();
			}
		}

		private function onNewBtnClick(event:MouseEvent):void {
			_nPlayersStepper.enabled = false;
			_moveSecStepper.enabled  = false;
			_totalMinStepper.enabled = false;
			_newBtn.enabled          = false;

			var baseConfig = {
				nPlayers: _nPlayersStepper.value,
				moveSec:  _moveSecStepper.value,
				totalMin: _totalMinStepper.value
			};
			_container.config(baseConfig, null);
		}

		// --------------------------------------------------------------------------

		private function onMouseDown(event:MouseEvent):void {
			parent.setChildIndex(this, parent.numChildren - 1);
			if (!_playerList.contains(event.target as DisplayObject)) {
				startDrag();
			}
		}

		private function onMouseUp(event:MouseEvent):void {
			if (!_playerList.contains(event.target as DisplayObject)) {
				stopDrag();
			}
		}

		// --------------------------------------------------------------------------

		private function _(id:String):String {
			return LoadScreen.clientGetText._(id);
		}
	}
}