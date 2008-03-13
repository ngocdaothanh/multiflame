package multiflame.container {
	import flash.display.*;
	import flash.events.*;
	import flash.text.*;
	import flash.net.URLVariables;

	import multiflame.utils.*;
	import multiflame.game.Constants;

	public class EmbedTab extends Sprite {
		private static const SWF_WIDTH:int = 500;
		private static const SWF_HEIGHT:int = 560;

		private var _channel:Channel;
		
		public function EmbedTab():void {
			_channel = Channel.instance;

			addEventListener(Event.ADDED, onAdded);
			_thisChannelEmbed.addEventListener(MouseEvent.CLICK, selectAll);
			_yourChannelEmbed.addEventListener(MouseEvent.CLICK, selectAll);
			_yourChannelName.addEventListener(KeyboardEvent.KEY_UP, onNameKeyUp);

			_yourChannelName.border = true;
		}
		
		private function onAdded(event:Event):void {
			if (event.target != this) {
				return;
			}

			var u:String = "http://" + Config.WEB_SITE;
			var l:String = '<a href="' + u + '">' + u + '</a>';
			_linkLbl.htmlText = "<b>" + StringUtil.substitute(_("More games at {0}"), l) + "</b>";

			_thisChannelLbl.htmlText = "<b>" + _("This channel") + "</b>";
			_thisChannelNameLbl.htmlText = _("Channel name");
			_thisChannelName.text = _channel.channel;
			_thisChannelURLLbl.htmlText = _("URL");
			_thisChannelURL.htmlText = webLink(_channel.channel);
			_thisChannelEmbedLbl.htmlText = _("Embed");
			_thisChannelEmbed.text = embed(_channel.channel);

			_yourChannelLbl.htmlText = "<b>" + _("Your channel") + "</b>";
			_yourChannelNameLbl.htmlText = _("Channel name");
			_yourChannelName.text = _("(can be anything)");
			_yourChannelURLLbl.htmlText = _("URL");
			_yourChannelURL.htmlText = "";
			_yourChannelEmbedLbl.htmlText = _("Embed");
			_yourChannelEmbed.text = "";
		}

		// --------------------------------------------------------------------------

		private function selectAll(event:MouseEvent):void {
			if (event.target == _thisChannelEmbed) {
				_thisChannelEmbed.setSelection(0, _thisChannelEmbed.text.length);
			} else if (event.target == _yourChannelEmbed) {
				_yourChannelEmbed.setSelection(0, _yourChannelEmbed.text.length);
			}
		}
		
		private function onNameKeyUp(event:KeyboardEvent):void {
			_yourChannelURL.htmlText = webLink(_yourChannelName.text);
			_yourChannelEmbed.text = embed(_yourChannelName.text);
		}

		private function webUrl(channel:String):String {
			channel = StringUtil.trim(channel);
			if (channel == "") {
				return "";
			}
			return "http://" + Config.WEB_SITE + "/games/" + _channel.id +
				"/" + encodeURL(channel) + "/" + _channel.locale;
		}

		private function swfUrl(channel:String):String {
			channel = StringUtil.trim(channel);
			if (channel == "") {
				return "";
			}
			return "http://" + Config.WEB_SITE + "/cwov/" + _channel.id +
				"/" + encodeURL(channel) + "/" + _channel.locale;
		}

		private function encodeURL(string:String):String {
			var v:URLVariables = new URLVariables();
			v.string = string;
			var s:String = v.toString();
			return s.slice(7);
		}

		private function webLink(channel:String):String {
			var u = webUrl(channel);
			if (u == "") {
				return "";
			}
			return '<a href="' + u + '">' + u + '</a>';
		}

		private function embed(channel:String):String {
			var u = swfUrl(channel);
			if (u == "") {
				return "";
			}
			return '<object width="' + SWF_WIDTH + '" height="' + SWF_HEIGHT +
				'"><param name="movie" value="' + u + '"></param>' +
				'<param name="wmode" value="transparent"></param>' +
				'<embed src="' + u + '" type="application/x-shockwave-flash" wmode="transparent" ' +
				'width="' + SWF_WIDTH + '" height="' + SWF_HEIGHT + '"></embed></object>'
		}

		private function _(id:String):String {
			return LoadScreen.clientGetText._(id);
		}
	}
}