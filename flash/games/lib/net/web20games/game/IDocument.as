package net.web20games.game {
	/**
	 * Game document MUST implements IDocument and MUST NOT be a subclass of Game.
	 */
	public interface IDocument {
		/**
		 * @return
		 * {
		 *   klass,      The game class
		 *   instance    The game instance used in the game document
		 * }
		 */
		function get game():Object;
	}
}