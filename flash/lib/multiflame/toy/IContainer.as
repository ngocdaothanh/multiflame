package multiflame.toy {
	public interface IContainer {
		/**
		 * Translates the given string into language indicated by the current locale.
		 */
		function _(id:String):String;

		/**
		 * @return
		 * The embedable tag.
		 */
		function embed(config:Array):String;
	}
}
