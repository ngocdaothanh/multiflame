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
import org.red5.server.api.so.ISharedObject; 

/**
 * Lobby: /<app>/<game>/<channel>
 * Room:  /<app>/<game>/<channel>/<player>
 */
public class Application extends ApplicationAdapter {
	protected final Logger logger = LoggerFactory.getLogger(Application.class);

	/* -----------------------------------------------------------------------*/

	private Auth auth;

	public void setAuth(Auth auth) {
		this.auth = auth;
	}

	/* -----------------------------------------------------------------------*/

	public boolean appStart() {
		logger.info("appStart");
		return true;
	}

	public void appStop() {
		logger.info("appStop");
	}

	/* -----------------------------------------------------------------------*/

	public boolean appConnect(IConnection conn, Object[] params) {
		return auth.appConnect(conn, params);
	}

	public void appDisconnect(IConnection conn) {
	}

	/* -----------------------------------------------------------------------*/

	public boolean roomStart(IScope room) {
		System.out.println("hehhehehehehheheheheehehhehehhehehe");
		Object handler = new Service();
		room.registerServiceHandler("sample", handler);
		

		createSharedObject(room, "players", false);
		ISharedObject so = getSharedObject(room, "players");
		so.setAttribute("names", "hahahaha");
		System.out.println(so);
		return true;
	}
}
