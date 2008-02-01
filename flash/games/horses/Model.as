package {
	import net.web20games.game.Model;

	public class Model extends net.web20games.game.Model {
		public var lastMove:Object;

		public function Model(baseConfig:Object, extendedConfig:Object):void {
			super(baseConfig, extendedConfig);
		}
		
		// --------------------------------------------------------------------------

		public override function get baseConfigRanges():Object {
			return {
				nPlayersMin: 2,
				nPlayersMax: 4,
				oneMoveTimeMin: 20,
				oneMoveTimeMax: 60,
				totalTimeMin: 0,
				totalTimeMax: 0,
				batchMode: false};
		}

		public override function onMove(index:int, data:Object):int {
			lastMove = {index: index, data: data};
			return net.web20games.game.Model.ANY_PLAYER;
		}

		public override function onLeave(index:int):int {
			return GAME_OVER;
		}

		public override function onTimeout():int {
			return GAME_OVER;
		}
	}
}