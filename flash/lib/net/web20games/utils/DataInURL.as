package net.web20games.utils {
	public class DataInURL {
		public static function encode(str:String):String {
			var encoder:DataInURL = new DataInURL();
			return encoder.encodeBase64(str);
		}

		public static function decode(str:String):String {
			var decoder:DataInURL = new DataInURL();
			return decoder.decodeBase64(str);
		}

		private static  var _endOfInput = -1;

		private static  var _chars:Array = new Array(
			'A','B','C','D','E','F','G','H',
			'I','J','K','L','M','N','O','P',
			'Q','R','S','T','U','V','W','X',
			'Y','Z','a','b','c','d','e','f',
			'g','h','i','j','k','l','m','n',
			'o','p','q','r','s','t','u','v',
			'w','x','y','z','0','1','2','3',
			'4','5','6','7','8','9','+','/'
		);

		private static var _charsReverseLookup:Array;
		private static var _charsReverseLookupInited:Boolean = initReverseChars();
		private static function initReverseChars():Boolean {
			_charsReverseLookup = new Array();

			for (var i=0; i < _chars.length; i++) {
				_charsReverseLookup[_chars[i]] = i;
			}

			return true;
		}

		private var _base64Str:String;
		private var _base64Count:Number;

		private function setBase64Str(str:String) {
			_base64Str = str;
			_base64Count = 0;
		}

		private function readBase64():Number {
			if (!_base64Str) {
				return _endOfInput;
			}

			if (_base64Count >= _base64Str.length) {
				return _endOfInput;
			}

			var c:Number = _base64Str.charCodeAt(_base64Count) & 0xff;
			_base64Count++;

			return c;
		}

		private function encodeBase64(str:String) {
			setBase64Str(str);
			var result = "";
			var inBuffer = new Array(3);
			var lineCount = 0;
			var done = false;

			while (!done && (inBuffer[0] = readBase64()) != _endOfInput) {
				inBuffer[1] = readBase64();
				inBuffer[2] = readBase64();

				result += (_chars[inBuffer[0] >> 2]);

				if (inBuffer[1] != _endOfInput) {
					result += (_chars[((inBuffer[0] << 4) & 0x30) | (inBuffer[1] >> 4)]);
					if (inBuffer[2] != _endOfInput) {
						result += (_chars[((inBuffer[1] << 2) & 0x3c) | (inBuffer[2] >> 6)]);
						result += (_chars[inBuffer[2] & 0x3F]);
					} else {
						result += (_chars[((inBuffer[1] << 2) & 0x3c)]);
						result += ("=");
						done = true;
					}
				} else {
					result += (_chars[((inBuffer[0] << 4) & 0x30)]);
					result += "=";
					result += "=";
					done = true;
				}

				lineCount += 4;

				if (lineCount >= 76) {
					result += ('\n');
					lineCount = 0;
				}
			}
			return result;
		}

		private function readReverseBase64() {
			if (!_base64Str) {
				return _endOfInput;
			}

			while (true) {
				if (_base64Count >= _base64Str.length) {
					return _endOfInput;
				}

				var nextCharacter:String = _base64Str.charAt(_base64Count);

				_base64Count++;

				if (_charsReverseLookup[nextCharacter]) {
					return _charsReverseLookup[nextCharacter];
				}

				if (nextCharacter == 'A') {
					return 0;
				}
			}

			return _endOfInput;
		}

		private function ntos(n:Number):String {
			var str:String = n.toString(16);

			if (str.length == 1) {
				str = "0" + str;
			}
			str = "%" + str;

			return unescape(str);
		}

		private function decodeBase64(str:String):String {
			setBase64Str(str);
			var result:String = "";
			var inBuffer:Array = new Array(4);
			var done:Boolean = false;

			while (!done && (inBuffer[0] = readReverseBase64()) != _endOfInput
			&& (inBuffer[1] = readReverseBase64()) != _endOfInput) {
				inBuffer[2] = readReverseBase64();
				inBuffer[3] = readReverseBase64();

				result += ntos((((inBuffer[0] << 2) & 0xff) | inBuffer[1] >> 4));

				if (inBuffer[2] != _endOfInput) {
					result += ntos((((inBuffer[1] << 4) & 0xff) | inBuffer[2] >> 2));
					if (inBuffer[3] != _endOfInput) {
						result +=  ntos((((inBuffer[2] << 6)  & 0xff) | inBuffer[3]));
					} else {
						done = true;
					}
				} else {
					done = true;
				}
			}

			return result;
		}
	}
}