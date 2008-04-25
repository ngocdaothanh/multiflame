package {
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.events.TimerEvent;
	import flash.text.TextField;
	import flash.display.Bitmap;
	import flash.display.BitmapData;
	import flash.utils.Timer;

	import org.cove.ape.*;

	import net.web20games.game.IGame;
	import net.web20games.game.IContainer;
	import net.web20games.game.Constants;

	public class Game extends Sprite implements IGame {
		private static const N_P:int = 4;   // Number of pieces (soccer players) of a player
		private static const R_P:int = 10;  // Radius of a player
		private static const R_B:int = 10;  // Radius of the ball

		private static const MAX_SHOOT_SPEED:int = 5;
		private static const ZERO_SPEED:Number = 0.1;  // Below this speed is seen as zero
		private static const MAX_STEP:int = 60;

		// Teams and the ball
		private var _teams:Array;
		private var _b:CircleParticle;

		// Index of the focused piece and velocities of pieces of this player,
		// only used if this player has joined the game
		private var _ip:int;    // 0...N_P

		// Steps of APE
		private var _steps:int;

		private var _container:IContainer;

		// ---------------------------------------------------------------------------

		public function Game():void {
			_nick0.selectable = false;
			_nick1.selectable = false;
			_nick0.htmlText = "";
			_nick1.htmlText = "";

			APEngine.init(1/4);
			APEngine.container = _field;

			var group:Group = new Group();
			group.collideInternal = true;
			APEngine.addGroup(group);

			// Field sides
			// Horizontal
			group.addParticle(new RectangleParticle(250, 0, 480, 2, 0, true));
			group.addParticle(new RectangleParticle(250, 360, 480, 2, 0, true));
			// Left vertical
			group.addParticle(new RectangleParticle(10, 65, 2, 130, 0, true));
			group.addParticle(new RectangleParticle(10, 295, 2, 130, 0, true));
			group.addParticle(new RectangleParticle(0, 180, 2, 100, 0, true));
			// Right vertical
			group.addParticle(new RectangleParticle(490, 65, 2, 130, 0, true));
			group.addParticle(new RectangleParticle(490, 295, 2, 130, 0, true));
			group.addParticle(new RectangleParticle(500, 180, 2, 100, 0, true));

			// Ball and pieces
			_b = new CircleParticle(100, 100, R_B, false, 1, 0.3, 0.3);
			_b.setDisplay(newBall());
			group.addParticle(_b);
			_teams = new Array(2);
			for (var i:int = 0; i < 2; i++) {
				_teams[i] = new Array(N_P);
				for (var j:int = 0; j < N_P; j++) {
					_teams[i][j] = new CircleParticle(100, 100, R_P, false, 3, 0.3, 0.3);
					_teams[i][j].setDisplay(newP(i));
					group.addParticle(_teams[i][j]);
				}
			}
			resetBallAndPieces();
			_steps = MAX_STEP;
			APEngine.paint();

			_field.addEventListener(MouseEvent.CLICK, onMClick);
			_field.addEventListener(MouseEvent.MOUSE_MOVE, onMMove);

			var t:Timer = new Timer(60);
			t.addEventListener(TimerEvent.TIMER, runAPE);
			t.start();
		}

		public function get container():IContainer {
			return _container;
		}

		// ---------------------------------------------------------------------------

		public function get definition():Object {
			return {
				type: Constants.WEGO,
				nPlayersMin: 2,
				nPlayersMax: 2,
				moveSecMin: 15,
				moveSecMax: 60,
				totalMinMin: 0,
				totalMinMax: 0
			};
		}

		public function set enabled(value:Boolean):void {
		}

		public function setContainer(container:IContainer):Object {
			_container = container;
			return {introSprite: new IntroSprite(this)};
		}

		public function onNewGame(snapshot:Object):int {
			// Display nicks
			_nick0.htmlText = _container.nicks0[0];
			_nick1.htmlText = _container.nicks0[1];

			//
			resetBallAndPieces();
			_steps = MAX_STEP;
			APEngine.paint();

			if (_container.indexMe >= 0) {
				if (_container.defaultMove == null) {
					_container.defaultMove = new Array(N_P);
				}
				for (var i:int = 0; i < N_P; i++) {
					_container.defaultMove[i] = [0, 0];
				}
				_ip = 0;
			}

			return Constants.ANY;
		}

		public function onMove(timestamp:Number, moves:Array):void {
			_field.graphics.clear();
			var indices:Array = [moves[0], moves[2]];
			var vss:Array = [moves[1], moves[3]];
			for (var i:int = 0; i < 2; i++) {
				var index:int = indices[i];
				for (var j:int = 0; j < N_P; j++) {
					_teams[index][j].velocity =
						limitShootSpeed(vss[i][j][0], vss[i][j][1]);
				}
			}
			_b.velocity = new Vector(0, 0);

			for (i = 0; i < N_P; i++) {
				_container.defaultMove[i] = [0, 0];
			}
			_ip = 0;

			_steps = 0;
			_container.setActionResult(Constants.ANY);
		}

		public function onResign(timestamp:Number, index:int):void {
			_container.gameResult[index] = Constants.LOST;
			_container.gameResult[1 - index] = Constants.WON;
			_container.setActionResult(Constants.OVER);
		}

		public function onTimeout(timestamp:Number, timedOut:Boolean, index:int):void {
			_container.gameResult[index] = Constants.LOST;
			_container.gameResult[1 - index] = Constants.WON;
			_container.setActionResult(Constants.OVER);
		}

		// ---------------------------------------------------------------------------

		private function runAPE(event:Event):void {
			if (_steps < MAX_STEP) {
				_steps++;
				APEngine.step();
				APEngine.paint();

				checkBall();
			}
		}

		private function onMClick(event:MouseEvent):void {
			if (_container == null || !_container.enabled ||
					_steps < MAX_STEP || event.target != _field) {
				return;
			}

			updateVelocity(event.localX, event.localY);

			if (_ip == 3) {
				_container.enqueueMove(_container.defaultMove);
			} else {
				_ip++;
			}
		}

		private function onMMove(event:MouseEvent):void {
			if (_container == null || !_container.enabled ||
					_steps < MAX_STEP || event.target != _field) {
				return;
			}

			_field.graphics.clear();
			_field.graphics.lineStyle(1);
			_field.graphics.beginFill(0xFF0000);
			for (var i:int = 0; i < _ip; i++) {
				_field.graphics.moveTo(_teams[_container.indexMe][i].px,
					_teams[_container.indexMe][i].py);
				_field.graphics.lineTo(_teams[_container.indexMe][i].px + _container.defaultMove[i][0],
					_teams[_container.indexMe][i].py + _container.defaultMove[i][1]);
			}
			_field.graphics.moveTo(_teams[_container.indexMe][_ip].px,
				_teams[_container.indexMe][_ip].py);
			_field.graphics.lineTo(event.localX, event.localY);
			_field.graphics.endFill();
		}

		// ---------------------------------------------------------------------------

		private function newBall():Sprite {
			var d:BitmapData = new BallBitmapData(20, 20);
			var b:Bitmap = new Bitmap(d);
			b.x = -(d.width/2);
			b.y = -(d.height/2);
			var s:Sprite =  new Sprite();
			s.addChild(b);
			return s;
		}

		private function newP(index:int):Sprite {
			var d:BitmapData = (index == 0)? new P0BitmapData(20, 20) : new P1BitmapData(20, 20);
			var b:Bitmap = new Bitmap(d);
			b.x = -(d.width/2);
			b.y = -(d.height/2);
			var s:Sprite =  new Sprite();
			s.addChild(b);
			return s;
		}

		// Reset players and ball positions.
		private function resetBallAndPieces():void {
			_b.px = 250;
			_b.py = 180;

			_teams[0][0].px = 90;  _teams[0][0].py = 140;
			_teams[0][1].px = 90;  _teams[0][1].py = 220;
			_teams[0][2].px = 165; _teams[0][2].py = 140;
			_teams[0][3].px = 165; _teams[0][3].py = 220;

			_teams[1][0].px = 335; _teams[1][0].py = 140;
			_teams[1][1].px = 335; _teams[1][1].py = 220;
			_teams[1][2].px = 410; _teams[1][2].py = 140;
			_teams[1][3].px = 410; _teams[1][3].py = 220;
		}

		private function updateVelocity(mouseX:int, mouseY:int):void {
			_container.defaultMove[_ip][0] = mouseX - _teams[_container.indexMe][_ip].px;
			_container.defaultMove[_ip][1] = mouseY - _teams[_container.indexMe][_ip].py;
		}

		private function limitShootSpeed(vx:Number, vy:Number):Vector {
			vx /= 40;
			vy /= 40;
			var r:Number;
			var ms2:Number = MAX_SHOOT_SPEED*MAX_SHOOT_SPEED;
			var s2:Number = vx*vx + vy*vy;
			if (s2 > ms2) {
				r = Math.sqrt(ms2/s2);
				vx *= r;
				vy *= r;
			}
			return new Vector(vx, vy);
		}

		private function checkBall():void {
			var iWon:int = -1;
			if (_b.px < R_B + 2) {
				iWon = 1;
			} else if (_b.px > 500 - R_B - 2) {
				iWon = 0;
			}
			if (iWon >= 0) {
				_steps = MAX_STEP;
				_container.gameResult[iWon] = Constants.WON;
				_container.gameResult[1 - iWon] = Constants.LOST;
				_container.setActionResult(Constants.OVER);
			}

			// Everything stopped?
			for (var i:int = 0; i < 2; i++) {
				for (var j:int = 0; j < N_P; j++) {
					var v:Vector = _teams[i][j].velocity;
					if (v.magnitude() > ZERO_SPEED) {
						return;
					}
				}
			}
			v = _b.velocity;
			if (v.magnitude() > ZERO_SPEED) {
				return;
			}
			_steps = MAX_STEP;
		}
	}
}