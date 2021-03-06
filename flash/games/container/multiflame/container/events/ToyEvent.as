﻿package multiflame.container.events {
	import flash.display.Sprite;
	import flash.events.Event;

	public class ToyEvent extends Event {
		public static const CONF:String = "TOY_EVENT_CONF";
		public static const SWF:String = "TOY_EVENT_SWF";

		// CONF
		public var toyId:int;
		public var config:XML;

		// SWF
		public var swf:Sprite;

		public function ToyEvent(type:String):void {
			super(type);
		}
	}
}