package fifo;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.red5.server.api.IConnection;
import org.red5.server.api.IScope;

public class Auth {
	private final Logger logger = LoggerFactory.getLogger(Auth.class);

	public boolean appConnect(IConnection conn, Object[] params) {
		// Only allow one connection per client
		if (conn.getClient().getConnections().size() > 1) {
			return false;
		}

		// Only allow the client to connect to the lobby:
		// /default/<app>/<game>/<channel>
		IScope scope = conn.getScope();
		if (scope.getDepth() != 3) {
			return false;
		}

		// User name must not contain /
		conn.setAttribute("userName", "FIXME");
		return true;
	}
}
