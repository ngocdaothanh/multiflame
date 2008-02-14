package multiflame.utils {
	public class StringUtil {
		// Returns true if the character is a whitespace character
		public static function isWhitespace(ch:String):Boolean {
			return ch == '\r' || ch == '\n' || ch == '\f' || ch == '\t' || ch == ' ';
		}

		public static function trim(original:String):String {
			// Split the string into an array of characters.
			var characters:Array = original.split("");

			// Remove any whitespace elements from the beginning of the array using
			// splice(). Use a break statement to exit the loop when you reach a
			// non-whitespace character to prevent it from removing whitespace
			// in the middle of the string.
			for (var i:int = 0; i < characters.length; i++) {
				if (isWhitespace(characters[i])) {
					characters.splice(i, 1);
					i--;
				} else {
					break;
				}
			}

			// Loop backward through the array removing whitespace elements until a
			// non-whitespace character is encountered. Then break out of the loop.
			for (i = characters.length - 1; i >= 0; i--) {
				if (isWhitespace(characters[i])) {
					characters.splice(i, 1);
				} else {
					break;
				}
			}

			// Recreate the string with the join() method and return the result.
			return characters.join("");
		}

		public static function substitute(str:String, ... rest):String {
			var len:uint = rest.length;
			var args:Array;
			if (len == 1 && rest[0] is Array) {
				args = rest[0] as Array;
				len = args.length;
			} else {
				args = rest;
			}
			for (var i:int = 0; i < len; i++) {
				str = str.replace(new RegExp("\\{"+i+"\\}", "g"), args[i]);
			}

			return str;
		}
	}
}