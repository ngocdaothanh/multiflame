package {
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.text.TextField;
	import flash.text.TextFormat;
	import flash.utils.Timer;
	import flash.events.TimerEvent;

	import org.cove.ape.*;

	import net.web20games.game.IGame;
	import net.web20games.game.IContainer;
	import net.web20games.game.Constants;

	public class Game extends Sprite implements IGame {
		// The first ball is the white one
		private static const N_B:int = 11;  // Number of balls
		private static const R_B:int = 10;  // Radius of a ball

		private static const R_H:int = 16;  // Radius of a hole
		private static const L_H:Number = 2*R_H*Math.sin(Math.PI/4);
		private static const L_W:int = (500 - 2*R_H - 2*L_H)/2;  // Wall length
		private static const T_W:int = 12;  // Wall thickness

		private static const MAX_SHOOT_SPEED:int = 16;
		private static const MAX_STEP:int = 300;

		private var _nick0:TextField;
		private var _nick1:TextField;

		private var _table:Sprite;

		private var _steps:int;  // Used to stop APE
		private var _wallGroup:Group;
		private var _ballGroup:Group;

		private var _whiteOutside:Boolean;
		private var _nonWhiteOutside:Boolean;

		// Points for each player
		private var _points0:int;
		private var _points1:int;

		private var _container:IContainer;

		// ---------------------------------------------------------------------------

		public function Game():void {
			_table = new Sprite();
			drawTableBackground();
			_table.y = (500 - 2*L_H - L_W)/2;
			addChild(_table);
			_table.addEventListener(MouseEvent.CLICK, onMClick);
			_table.addEventListener(MouseEvent.CLICK, onMClick);
			_table.addEventListener(MouseEvent.MOUSE_MOVE, onMMove);

			var format:TextFormat = new TextFormat();
			format.font = "_sans";
			format.size = 12;

			_nick0 = new TextField();
			_nick1 = new TextField();

			_nick0.defaultTextFormat = format;
			_nick1.defaultTextFormat = format;
			_nick0.selectable = false;
			_nick1.selectable = false;
			_nick0.htmlText = "";
			_nick1.htmlText = "";

			_nick0.x = 300;
			_nick0.y = _table.y - 40;
			_nick1.x = _nick0.x;
			_nick1.y = _table.y + 2*L_H + L_W + 20;
			_nick0.height = 20;
			_nick1.height = 20;
			_nick0.width = 500;
			_nick1.width = 500;
			addChild(_nick0);
			addChild(_nick1);

			APEngine.init(1/4);
			APEngine.container = _table;
			APEngine.damping = 0.99;
			APEngine.constraintCollisionCycles = 2;

			// Walls
			_wallGroup = new Group();
			APEngine.addGroup(_wallGroup);
			// Top horizontal
			_wallGroup.addParticle(new RectangleParticle(L_H + L_W/2, 0, L_W, T_W, 0, true));
			_wallGroup.addParticle(new RectangleParticle(L_H + L_W + 2*R_H + L_W/2, 0, L_W, T_W, 0, true));
			// Bottom horizontal
			_wallGroup.addParticle(new RectangleParticle(L_H + L_W/2, 2*L_H + L_W, L_W, T_W, 0, true));
			_wallGroup.addParticle(new RectangleParticle(L_H + L_W + 2*R_H + L_W/2, 2*L_H + L_W, L_W, T_W, 0, true));
			// Left vertical
			_wallGroup.addParticle(new RectangleParticle(0, L_H + L_W/2, T_W, L_W, 0, true));
			// Right vertical
			_wallGroup.addParticle(new RectangleParticle(500, L_H + L_W/2, T_W, L_W, 0, true));

			resetBallGroup();
			_steps = MAX_STEP;
			APEngine.paint();

			var t:Timer = new Timer(16);
			t.addEventListener(TimerEvent.TIMER, runAPE);
			t.start();
		}

		public function get container():IContainer {
			return _container;
		}

		// ---------------------------------------------------------------------------

		public function get definition():Object {
			return {
				klass: Constants.TURN_BASED,
				nPlayersMin: 2,
				nPlayersMax: 2,
				moveSecMin: 30,
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

			// Reset
			graphics.clear();  // Clear won balls
			_whiteOutside = false;
			_nonWhiteOutside = false;
			_points0 = 0;
			_points1 = 0;
			resetBallGroup();
			_steps = MAX_STEP;
			APEngine.paint();

			return 0;
		}

		public function onMove(timestamp:Number, moves:Array):void {
			var index:int = moves[0];
			var v:Array = moves[1];
			var whiteBall:CircleParticle = _ballGroup.getAll()[0];
			whiteBall.velocity = limitShootSpeed(new Vector(v[0], v[1]));
			_steps = 0;
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

		// --------------------------------------------------------------------------

		private function runAPE(event:Event):void {
			if (_steps < MAX_STEP) {
				_steps++;
				APEngine.step();
				APEngine.paint();

				checkOutside();

				if (_steps == MAX_STEP) {
					resetSpeed();
					// For example, when a player resigns, the game is over before APE stops
					if (_container.lastActionResult != Constants.OVER) {
						checkResult();
					}
				}
			}
		}

		private function onMClick(event:MouseEvent):void {
			if (_container == null || !_container.enabled || _steps < MAX_STEP || event.target != _table) {
				return;
			}

			drawTableBackground();
			var whiteBall:CircleParticle = _ballGroup.getAll()[0];
			var v:Vector = limitShootSpeed(new Vector(
				(event.localX - whiteBall.px)/20.0, (event.localY - whiteBall.py)/20.0));
			_container.enqueueMove([v.x, v.y]);
		}

		private function onMMove(event:MouseEvent):void {
			if (_container == null || !_container.enabled || _steps < MAX_STEP || event.target != _table) {
				return;
			}

			drawTableBackground();
			var whiteBall:CircleParticle = _ballGroup.getAll()[0];
			_table.graphics.beginFill(0xCCCCCC);
			_table.graphics.lineStyle(1);
			_table.graphics.moveTo(whiteBall.px, whiteBall.py);
			_table.graphics.lineTo(event.localX, event.localY);
			_table.graphics.endFill();
		}

		// ---------------------------------------------------------------------------

		private function resetBallGroup():void {
			var i:int;

			// Remove all sprites attached to the old _ballGroup
			if (_ballGroup != null) {
				var a:Array = _ballGroup.getAll();
				for (i = 0; i < a.length; i++) {
					if (_table.contains(a[i].sprite)) {
						_table.removeChild(a[i].sprite);
					}
				}
				APEngine.removeGroup(_ballGroup);
			}

			// Create a new one
			_ballGroup = new Group(true);
			_ballGroup.addCollidable(_wallGroup);
			APEngine.addGroup(_ballGroup);

			// Add the white ball
			var b:CircleParticle = new CircleParticle(100, 100, R_B);
			_ballGroup.addParticle(b);
			resetWhiteBallPosition();

			// Add other balls
			for (i = 0; i < N_B - 1; i++) {
				b = new CircleParticle(150, R_H + L_W/2, R_B);
				var s:Sprite = new Sprite();
				s.graphics.clear();
				s.graphics.lineStyle(1);
				s.graphics.beginFill(0x333333);
				s.graphics.drawCircle(0, 0, R_B);
				s.graphics.endFill();
				b.setDisplay(s);
				_ballGroup.addParticle(b);
			}

			// For some reason, APE does not attach the sprites
			APEngine.container = _table;
			a = _ballGroup.getAll();
			for (i = 0; i < a.length; i++) {
				_table.addChild(a[i].sprite);
			}

			APEngine.paint();
		}

		private function drawTableBackground():void {
			_table.graphics.clear();
			_table.graphics.lineStyle(0);
			_table.graphics.beginFill(0xEEEEEE);
			_table.graphics.drawRect(0, 0, 500, 2*L_H + L_W);
			_table.graphics.endFill();
		}

		private function drawWonBall(index:int):void {
			var x:int;
			var y:int;
			if (index == 0) {
				x = _nick0.x - _points0*(R_B*2 + 5);
				y = _nick0.y + R_B;
			} else {
				x = _nick1.x - _points1*(R_B*2 + 5);
				y = _nick1.y + R_B;
			}
			graphics.lineStyle(1);
			graphics.beginFill(0x333333);
			graphics.drawCircle(x, y, R_B);
			graphics.endFill();
		}

		private function resetWhiteBallPosition():void {
			var whiteBall:CircleParticle = _ballGroup.getAll()[0];
			whiteBall.px = 500 - R_H - L_W/2;
			whiteBall.py = R_H + L_W/2;
			APEngine.paint();
		}

		// Set speed of all balls to 0
		private function resetSpeed():void {
			var balls:Array = _ballGroup.getAll();
			for (var i:int = 0; i < balls.length; i++) {
				if (balls[i] != null) {
					balls[i].velocity = new Vector(0, 0);
				}
			}
		}

		private function limitShootSpeed(v:Vector):Vector {
			var r:Number;
			if (v.magnitude() > MAX_SHOOT_SPEED) {
				r = MAX_SHOOT_SPEED/v.magnitude();
				v = v.times(new Vector(r, r));
			}
			return v;
		}

		// ---------------------------------------------------------------------------

		// Check if the balls have gone outside.
		private function checkOutside():void {
			var balls:Array = _ballGroup.getAll();
			for (var i:int = 0; i < balls.length; i++) {
				if (balls[i].px < 0 || balls[i].py < 0 ||
						balls[i].px > 500 || balls[i].py > 2*L_H + L_W) {
					if (i == 0) {
						_whiteOutside = true;
						resetWhiteBallPosition();
						_steps = MAX_STEP;  // Stop, resetSpeed() will be called later
						return;
					} else {
						_nonWhiteOutside = true;
						if (_container.lastActionResult == 0) {
							_points0++;
							drawWonBall(0);
						} else {
							_points1++;
							drawWonBall(1);
						}
						_ballGroup.removeParticle(balls[i]);
						// Not efficient, but works
						balls = _ballGroup.getAll();
						i = 0;
					}
				}
			}
		}

		private function checkResult():void {
			var half:int = (N_B - 1)/2;
			if (_points0 > half) {
				_container.gameResult[0] = Constants.WON;
				_container.gameResult[1] = Constants.LOST;
				_container.setActionResult(Constants.OVER);
				return;
			} else if (_points1 > half) {
				_container.gameResult[1] = Constants.WON;
				_container.gameResult[0] = Constants.LOST;
				_container.setActionResult(Constants.OVER);
				return;
			} 

			if (_ballGroup.getAll().length == 1) {
				if (_points0 > _points1) {
					_container.gameResult[0] = Constants.WON;
					_container.gameResult[1] = Constants.LOST;
				} else if (_points1 > _points0) {
					_container.gameResult[1] = Constants.WON;
					_container.gameResult[0] = Constants.LOST;
				} else {
					_container.gameResult[0] = Constants.DREW;
					_container.gameResult[1] = Constants.DREW;
				}
				_container.setActionResult(Constants.OVER);
				return;
			}

			if (_whiteOutside) {
				_whiteOutside = false;
				_nonWhiteOutside = false;
				_container.setActionResult(1 - _container.lastActionResult);
				return;
			}

			if (_nonWhiteOutside) {
				_nonWhiteOutside = false;
				_container.setActionResult(_container.lastActionResult);
			} else {
				_container.setActionResult(1 - _container.lastActionResult);
			}
		}
	}
}