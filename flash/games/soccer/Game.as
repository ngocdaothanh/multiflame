package {
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.text.TextField;

	import org.cove.ape.*;

	import net.web20games.game.Game;

	public class Game extends net.web20games.game.Game {
		private static const N_P:int = 4;   // Number of pieces (soccer players) of a player
		private static const R_P:int = 10;  // Radius of a player
		private static const R_B:int = 10;  // Radius of the ball

		private static const MAX_SHOOT_SPEED:int = 150;
		private static const MAX_STEP:int = 150;

		// Teams and the ball
		private var _p0:Array;
		private var _p1:Array;
		private var _b:CircleParticle;

		// Focused piece
		private var _p:CircleParticle;
		private var _ip:int;  // Index, 0...N_P

		// Steps of APE
		private var _steps:int;

		// Velocities for each piece, only used if this player has joined the game
		// [vx, vy, ...]
		private var _vs:Array;

		// ---------------------------------------------------------------------------

		public function Game():void {
			enabled = false;
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
			_b = new CircleParticle(100, 100, R_B);
			_b.setDisplay(new Ball());
			group.addParticle(_b);
			_p0 = new Array(N_P);
			_p1 = new Array(N_P);
			for (var i:int = 0; i < N_P; i++) {
				_p0[i] = new CircleParticle(100, 100, R_P);
				_p0[i].setDisplay(new P0());
				_p0[i].sprite.addEventListener(MouseEvent.CLICK, onMClick);
				group.addParticle(_p0[i]);

				_p1[i] = new CircleParticle(100, 100, R_P);
				_p1[i].setDisplay(new P1());
				_p1[i].sprite.addEventListener(MouseEvent.CLICK, onMClick);
				group.addParticle(_p1[i]);
			}
			resetBallAndPieces();

			addEventListener(Event.ENTER_FRAME, runAPE);
			_field.addEventListener(MouseEvent.CLICK, onMClick);
			_field.addEventListener(MouseEvent.MOUSE_MOVE, onMMove);
		}

		// ---------------------------------------------------------------------------

		public override function get definition():Object {
			return {
				klass: CLASS_BATCH,
				nPlayersMin: 2,
				nPlayersMax: 2,
				moveSecMin: 15,
				moveSecMax: 60,
				totalMinMin: 0,
				totalMinMax: 0
			};
		}

		protected override function onContainerSet():Object {
			return {introSprite: new IntroSprite(this)};
		}

		protected override function onNewGame(playedBack:Boolean):int {
			// Display nicks
			_nick0.htmlText = nicks0[0];
			_nick1.htmlText = nicks0[1];

			//
			if (indexMe >= 0) {
				if (_vs == null) {
					_vs = new Array(N_P*2);
				}
				for (var i:int = 0; i < N_P*2; i++) {
					_vs[i] = 0;
				}
			}
			resetBallAndPieces();

			return A_ANY;
		}

		protected override function onMove(timestamp:Number, moves:Array, playedBack:Boolean):int {
			return A_ANY;
		}

		protected override function onResign(timestamp:Number, index:int, playedBack:Boolean):int {
			updateGameResult(index, P_LOST);
			updateGameResult(1 - index, P_WON);
			return A_OVER;
		}

		protected override function onTimeout(timestamp:Number, timedOut:Boolean, index:int, playedBack:Boolean):int {
			updateGameResult(index, P_LOST);
			updateGameResult(1 - index, P_WON);
			return A_OVER;
		}

		// ---------------------------------------------------------------------------

		private function runAPE(event:Event):void {
			if (_steps < MAX_STEP) {
				_steps++;
				APEngine.step();
				APEngine.paint();
			}
		}

		private function onMClick(event:MouseEvent):void {
			if (indexMe < 0 || !enabled || _steps < MAX_STEP) {
				return;
			}

			// Set velocity for the focused piece
			if (event.target == _field && _p != null) {
				updateVelocity();
				_p = null;
				return;
			}

			// Determine the focused piece
			var i:int;
			if (event.target is P0) {
				if (indexMe != 0) {
					return;
				}
				for (i = 0; i < N_P; i++) {
					if (_p0[i].sprite.contains(event.target)) {
						_p = _p0[i];
						_ip = i;
						break;
					}
				}
			} else if (event.target is P1) {
				if (indexMe != 1) {
					return;
				}
				for (i = 0; i < N_P; i++) {
					if (_p1[i].sprite.contains(event.target)) {
						_p = _p1[i];
						_ip = i;
						break;
					}
				}
			}
		}

		private function onMMove(event:MouseEvent):void {
			if (indexMe < 0 || !enabled || event.target != _field || _p == null) {
				return;
			}

			_field.graphics.clear();
			_field.graphics.lineStyle(1);
			_field.graphics.beginFill(0xFF0000);
			_field.graphics.moveTo(_p.px, _p.py);
			_field.graphics.lineTo(event.localX, event.localY);
			_field.graphics.endFill();
		}

		// ---------------------------------------------------------------------------

		// Reset players and ball positions.
		private function resetBallAndPieces():void {
			_b.px = 250;
			_b.py = 180;

			_p0[0].px = 90;  _p0[0].py = 140;
			_p0[1].px = 90;  _p0[1].py = 220;
			_p0[2].px = 165; _p0[2].py = 140;
			_p0[3].px = 165; _p0[3].py = 220;

			_p1[0].px = 335; _p1[0].py = 140;
			_p1[1].px = 335; _p1[1].py = 220;
			_p1[2].px = 410; _p1[2].py = 140;
			_p1[3].px = 410; _p1[3].py = 220;

			// Force APE to repaint
			_steps = MAX_STEP - 1;
			runAPE(null);
		}

		private function updateVelocity():void {
			_field.graphics.clear();
			var vx:int = event.localX - _p.px;
			var vy:int = event.localY - _p.py;
			_p.velocity = limitShootSpeed(vx, vy);

			_vs[_ip*2] = _p.velocity.x;
			_vs[_ip*2 + 1] = _p.velocity.y;
		}

		private function limitShootSpeed(vx:int, vy:int):Vector {
			var r:Number;
			var ms2:Number = MAX_SHOOT_SPEED*MAX_SHOOT_SPEED;
			var s2:Number = vx*vx + vy*vy;
			if (s2 > ms2) {
				r = Math.sqrt(ms2/s2);
				vx = vx*r;
				vy = vy*r;
			}
			return new Vector(vx, vy);
		}
	}
}