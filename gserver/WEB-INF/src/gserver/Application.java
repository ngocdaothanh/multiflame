package gserver;

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
	private final Logger logger = LoggerFactory.getLogger(Application.class);

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
		if (!auth.appConnect(conn, params)) {
			return false;
		}

		// Not put in roomStart() because it is always run before appConnect().
		IScope lobbyScope = conn.getScope();
		Lobby.attach(this, lobbyScope);

		return true;
	}
}
