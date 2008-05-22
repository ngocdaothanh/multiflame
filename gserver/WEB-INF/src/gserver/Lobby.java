package gserver;

import org.red5.server.adapter.ApplicationAdapter;
import org.red5.io.amf3.ByteArray;
import org.red5.server.api.IConnection;
import org.red5.server.api.IScope;
import org.red5.server.api.Red5;
import org.red5.server.api.so.ISharedObject;

public class Lobby {
	private static ApplicationAdapter application;
	private static ISharedObject so;

	public static void attach(ApplicationAdapter app, IScope scope) {
		if (application == null) {
			application = app;
		}

		synchronized(scope) {
			Object lobbyHandler = scope.getServiceHandler("lobby");
			if (lobbyHandler == null) {
				application.createSharedObject(scope, "so", false);
				ISharedObject so = application.getSharedObject(scope, "so");
				lobbyHandler = new Lobby(so);
				scope.registerServiceHandler("lobby", lobbyHandler);
			}
		}
	}

	private Lobby(ISharedObject so) {
		this.so = so;
	}

	public void roomNew(IConnection conn) {
		IScope lobby = conn.getScope();

		// This user must currently be in lobby
		if (lobby.getDepth() != 3) {
			return;
		}

		String userName = (String) conn.getAttribute("userName");
		lobby.createChildScope(userName);
		IScope room = lobby.getScope(userName);
		conn.connect(room);
		Room.attach(application, room);
	}
}
