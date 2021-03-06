﻿package {
	import flash.display.*;
	import flash.text.TextField;
	import flash.events.*;
	import flash.media.*;
	import flash.geom.Rectangle;
	import flash.utils.*;

	import multiflame.toy.IToy;
	import multiflame.toy.IContainer;
	import multiflame.toy.Constants;
	import multiflame.utils.Config;
	
	public class Document extends Sprite implements IToy {
		public static const WIDTH:int  = 320;
		public static const HEIGHT:int = 280;

		private static const THRESHOLD:int = 320*240*2;

		private var _container:IContainer;

		private var _vid:Video;
		private var _bmd:BitmapData;
		private var _emailDlg:EmailDlg;
		private var _dogBark:Sound;

		public function Document():void {
			graphics.beginFill(0x333333);
			graphics.drawRect(0, 0, WIDTH, HEIGHT);
			graphics.endFill();
		}

		public function setContainer(container:IContainer, mode:int, params:Array):Array {
			var ret:Array = [WIDTH, HEIGHT];

			_container = container;

			removeChild(_toggleBtn);
			_copywrite.text = _("Antitheft camera") + " - " + Config.WEB_SITE;

			_status.selectable = false;
			_status.text = _("Please connect a webcam.");
			if (Camera.names.length == 0) {
				return ret;
			}

			var cam:Camera = Camera.getCamera();
			if (cam == null) {
				return ret;
			}

			_vid = new Video();
			_vid.smoothing = true;
			cam.addEventListener(StatusEvent.STATUS, onCameraAccept);
			_status.text = _("Please allow access to your webcam.");
			_vid.attachCamera(cam);
			addChild(_vid);

			_bmd = new BitmapData(_vid.width, _vid.height);

			return ret;
		}

		// ---------------------------------------------------------------------------

		private function _(id:String):String {
			if (_container == null) {
				return id;
			} else {
				return _container._(id);
			}
		}

		private function onCameraAccept(event:StatusEvent):void {
			if (event.code == "Camera.Unmuted") {
				_emailDlg = new EmailDlg(_container);
				_emailDlg.x = 50;
				_emailDlg.y = 50;
				_emailDlg.addEventListener(EmailDlg.OK, onEmailDlgOK);
				_status.text = "";
				addChild(_emailDlg);
			}
		}

		private function onEmailDlgOK(event:Event):void {
			removeChild(_emailDlg);
			_dogBark = new DogBark();

			_toggleBtn.border = true;
			_toggleBtn.selectable = false;
			addChild(_toggleBtn);
			_toggleBtn.addEventListener(MouseEvent.CLICK, onToggleBtnClick);
			onToggleBtnClick(null);
		}

		private function onToggleBtnClick(event:Event):void {
			if (_toggleBtn.text == "Stop") {
				_toggleBtn.text = _("Start");
				checkMovement();
			} else {
				_toggleBtn.text = _("Stop");
			}
			_status.text = "";
		}

		private function checkMovement():void {
			var bmd:BitmapData = new BitmapData(_vid.width, _vid.height);
			bmd.draw(_vid);
			if (_bmd == null) {
				_bmd = bmd;
				return;
			}

			if (_toggleBtn.text == _("Stop")) {
				var sum:int = 0;
				var p1, p2:uint;
				var r, g, b:int;
				var b1, b2:int;
				for (var row:int = 0; row < _vid.height; row += 2) {
					for (var col:int = 0; col < _vid.width; col += 2) {
						p2 = bmd.getPixel(row, col);
						p1 = _bmd.getPixel(row, col);
	
						r = (p2 >> 8*2) & 0x0000FF;
						g = (p2 >> 8) & 0x0000FF;
						b = p2 & 0x0000FF;
						b2 = 0.3*r + 0.3*g + 0.3*b;
	
						r = (p1 >> 8*2) & 0x0000FF;
						g = (p1 >> 8) & 0x0000FF;
						b = p1 & 0x0000FF;
						b1 = 0.3*r + 0.3*g + 0.3*b;
	
						sum += Math.abs(b2 - b1);
					}
				}
	
				//trace(sum);
				if (sum > THRESHOLD) {
					_status.text = _("Moved");
					_dogBark.play();
					_emailDlg.mail(_("Antitheft camera"), _("Captured image"), bmd);
				} else {
					_status.text = "";
				}
			}

			_bmd = bmd;
			setTimeout(checkMovement, 100);
		}
	}
}