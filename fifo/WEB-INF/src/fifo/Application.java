package fifo;

import java.util.Map;
import java.util.List;

//import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;

import org.red5.server.api.IConnection;
import org.red5.server.api.IClient;
import org.red5.server.api.IScope;
import org.red5.server.api.Red5;
import org.red5.server.api.service.IServiceCapableConnection;
import org.red5.server.adapter.ApplicationAdapter;
import org.red5.server.exception.ClientRejectedException;
import org.red5.io.amf3.ByteArray;

public class Application extends ApplicationAdapter {
	public boolean appStart() {
		System.out.println("1");
		return true;
	}

	public void appStop() {
		System.out.println("2");
	}

	public boolean appConnect(IConnection conn, Object[] params) {
		System.out.println("3");
		
		if (conn instanceof IServiceCapableConnection) {
	        IServiceCapableConnection sc = (IServiceCapableConnection) conn;
	        ByteArray ba = new ByteArray();
			ba.writeUTF("haha\n\nhehe");
	        sc.invoke("onret", new Object[]{ba});
	    }

		return true;
	}

	public void appDisconnect(IConnection conn) {
		System.out.println("4");
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
