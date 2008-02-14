package multiflame.container {
	import multiflame.game.Constants;

	public class TimeoutCalculator {
		private var _klass:int;
		private var _baseConfig:Object;
		private var _totalSecLefts:Array;

		private var _lastTimestamp:Number;
		private var _lastIndex:int;

		private var _moveSecLeft:Number;
		private var _totalSecLeft:Number;

		public function TimeoutCalculator(klass:int, baseConfig:Object, firstIndex:int):void {
			_klass = klass;
			_baseConfig = baseConfig;

			// Initialize _totalSecLefts to baseConfig.totalMin*60
			if (baseConfig.totalMin > 0) {
				_totalSecLefts = new Array(baseConfig.nPlayers);
				for (var i:int = 0; i < baseConfig.nPlayers; i++) {
					_totalSecLefts[i] = baseConfig.totalMin*60;
				}
			}

			_moveSecLeft = baseConfig.moveSec;
			_totalSecLeft = baseConfig.totalMin*60;

			_lastTimestamp = 0;
			_lastIndex = firstIndex;
		}

		// ---------------------------------------------------------------------------
		
		public function get moveSecLeft():int {
			if (_klass == Constants.REALTIME || _baseConfig.moveSec == 0) {
				return 0;
			}
			return _moveSecLeft;
		}

		public function get totalSecLeft():int {
			if (_baseConfig.totalMin == 0) {
				return 0;
			}
			return _totalSecLeft;
		}

		// ---------------------------------------------------------------------------

		/**
		 * Game#onMove/onResign/onTimeout -> [game processing] -> calc -> moveSecLeft, totalSecLeft
		 */
		public function calc(timestamp:Number, processingSec:Number, actionResult:int):void {
			if (actionResult == Constants.OVER) {
				_moveSecLeft = 0;
				_totalSecLeft = 0;
				return;
			}

			switch (_klass) {
			case Constants.TURN_BASED:
				_moveSecLeft = _baseConfig.moveSec - processingSec;				

				if (_totalSecLefts != null) {
					_totalSecLefts[_lastIndex] -= (timestamp - _lastTimestamp) + processingSec;
					_totalSecLeft = _totalSecLefts[actionResult];
				} else {
					_totalSecLeft = 0;
				}
				break;
			case Constants.REALTIME:
				if (_totalSecLefts != null) {
					_totalSecLeft = _baseConfig.totalMin*60 - timestamp - processingSec;
				}
				break;
			case Constants.BATCH:
				_moveSecLeft = _baseConfig.moveSec;
				if (_totalSecLefts != null) {
					_totalSecLeft = _baseConfig.totalMin*60 - timestamp - processingSec;
				}
				break;
			}

			_lastTimestamp = timestamp;
			_lastIndex = actionResult;
		}

		/**
		 * Will not be called for batch game.
		 * checkTimeout -> Game#onTimeout -> [game processing] -> calc -> moveSecLeft, totalSecLeft
		 */
		public function checkTimeout(timestamp:Number, reporterIndex:int):Array {
			var timedOut:Boolean = false;
			var index:int = reporterIndex;
			var dt:Number;

			switch (_klass) {
			case Constants.TURN_BASED:
				dt = timestamp - _lastTimestamp;
				if (dt >= _moveSecLeft ||
						(_totalSecLefts != null && dt >= _totalSecLefts[_lastIndex])) {
					timedOut = true;
					index = _lastIndex;
				}
				break;
			case Constants.REALTIME:
				dt = timestamp;
				if (dt >= _baseConfig.totalMin*60) {
					timedOut = true;
					index = -1;
				}
				break;
			}

			return [timedOut, index];
		}
	}
}