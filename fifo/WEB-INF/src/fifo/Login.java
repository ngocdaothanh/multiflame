package fifo;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.red5.server.api.IConnection;

public class Login {
	protected final Logger logger = LoggerFactory.getLogger(Login.class);

	private static final Login instance = new Login();

	private Login() {
	}

	public static Login getInstance() {
		return instance;
	}

	public boolean appConnect(IConnection conn, Object[] params) {
		logger.debug("6666");
		logger.info("infofnf");
		// Only allow one connection per client
		if (conn.getClient().getConnections().size() > 1) {
			return false;
		}
		return true;
	}
}
