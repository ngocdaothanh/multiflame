package net.web20games.game {
	public interface IContainer {
		/**
		 * Called by the config dialog.
		 */
		function get definition():Object;
		function init(baseConfig:Object, extendedConfig:Object):void;
		function join():void;
		function unjoin():void;

		/**
		 * Called by the game instance to make a move.
		 */
		function move(data:Object):void;

		/**
		 * Called by the game instance to translate text on view.
		 */
		function _(id:String):String;

		/**
		 * @return TweenLite class. See http://www.TweenLite.com.
		 */
		function get TweenLite():*;

		/**
		 * @return TweenFilerLite class. See http://www.TweenLite.com.
		 */
		function get TweenFilterLite():*;
	}
}