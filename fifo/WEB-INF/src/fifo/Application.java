package fifo;

import java.util.Map;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.red5.server.api.IConnection;
import org.red5.server.api.IClient;
import org.red5.server.api.IScope;
import org.red5.server.api.Red5;
import org.red5.server.api.service.IServiceCapableConnection;
import org.red5.server.adapter.ApplicationAdapter;
import org.red5.server.exception.ClientRejectedException;
import org.red5.io.amf3.ByteArray;

/**
 * Lobby: /<app>/<game>/<channel>
 * Room:  /<app>/<game>/<channel>/<player>
 */
public class Application extends ApplicationAdapter {
	protected final Logger logger = LoggerFactory.getLogger(Application.class);

	public boolean appStart() {
		logger.info("fifo appStart");
		return true;
	}

	public void appStop() {
		logger.info("fifo appStop");
	}

	/* -----------------------------------------------------------------------*/

	public boolean appConnect(IConnection conn, Object[] params) {
		logger.debug("xxx6666");
		logger.info("yyyinfofnf");
		return Login.getInstance().appConnect(conn, params);
	}

	public void appDisconnect(IConnection conn) {
	}

	/* -----------------------------------------------------------------------*/

	public boolean roomStart(IScope room) {
		return true;
	}
	public ByteArray amf3(String s) {
		System.out.println("5");
		System.out.println(s);
		System.out.println("5b");
		ByteArray ba = new ByteArray();
		ba.writeUTF("haha\n\nhehe");
		return ba;
	}
}
