package fifo;

import org.red5.server.adapter.ApplicationAdapter;
import org.red5.server.api.IScope;
import org.red5.server.api.so.ISharedObject;

public class Room {
	private static ISharedObject so;

	public static void attach(ApplicationAdapter app, IScope scope) {
		synchronized(scope) {
			Object roomHandler = scope.getServiceHandler("room");
			if (roomHandler == null) {
				app.createSharedObject(scope, "so", false);
				ISharedObject so = app.getSharedObject(scope, "so");
				roomHandler = new Room(so);
				scope.registerServiceHandler("room", roomHandler);
			}
		}
	}

	private Room(ISharedObject so) {
		this.so = so;
	}
}
