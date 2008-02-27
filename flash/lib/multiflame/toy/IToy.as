package multiflame.toy {
	public interface IToy {
		/**
		 * @param mode
		 * See Constants.as
		 *
		 * @return
		 * Array of [width, height]
		 */
		function setContainer(container:IContainer, mode:int, config:Array):Array;
	}
}
