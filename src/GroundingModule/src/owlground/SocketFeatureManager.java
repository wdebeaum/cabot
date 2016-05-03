package owlground;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;

import javax.xml.stream.XMLStreamException;

import owlground.spaces.SpaceManager;

public class SocketFeatureManager extends FeatureManager {

	int messagePort;
	int dataPort;
	String hostName;
	ServerSocket dataServerSocket;
	ServerSocket localMessageServerSocket; //If local, this is the server for messages
	Socket messageSocket;
	Socket dataSocket;
	boolean local;
	
	public SocketFeatureManager(SpaceManager spaceManager, String hostName, 
			int messagePort, int dataPort) {
		super(spaceManager);
		this.hostName = hostName;
		this.messagePort = messagePort;
		this.dataPort = dataPort;
		local = false;
		startSockets();
		
	}
	
	/**
	 * Use this if the TRIPS and the feature extraction are running on the same machine.
	 * @param spaceManager
	 * @param hostName
	 * @param messagePort
	 */
	public SocketFeatureManager(SpaceManager spaceManager, int messagePort) {
		super(spaceManager);
		this.hostName = "localhost";
		this.messagePort = messagePort;
		this.dataPort = messagePort;
		
		local = true;
		startSockets();
	}
	
	public void startSockets()
	{

		try {
			if (local)
				localMessageServerSocket = new ServerSocket(messagePort);
			else
				dataServerSocket = new ServerSocket(dataPort);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void sendXmlRequest(int uttNum)
	{
		long startTime = numberedUtterances.get(uttNum).getStartTime();
		long endTime = numberedUtterances.get(uttNum).getEndTime();
		
		try {
			if (local)
				messageSocket = localMessageServerSocket.accept();
			else
				messageSocket = new Socket("localhost", messagePort);
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			PrintWriter out = new PrintWriter(messageSocket.getOutputStream(), true);
			out.println(startTime + "\n" + endTime);
			out.flush();
			// If we are sending the frames, we'll close this socket and make a new one
			// But if we're local, we need to keep it open for the return message containing data
			if (!local)
			{
				messageSocket.close();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
	
	public void loadXmlFileFromSocket(int uttNum) throws IOException, XMLStreamException
	{
		sendXmlRequest(uttNum);
		
		Socket dataSocket = null;
		try {
			if (local)
				dataSocket = messageSocket;
			else
				dataSocket = dataServerSocket.accept();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		readXmlStream(dataSocket.getInputStream());
		if (!local)
			dataSocket.close();
		
		
	}

	public int getMessagePort() {
		return messagePort;
	}

	public void setMessagePort(int messagePort) {
		this.messagePort = messagePort;
	}

	public int getDataPort() {
		return dataPort;
	}

	public void setDataPort(int dataPort) {
		this.dataPort = dataPort;
	}

	public String getHostName() {
		return hostName;
	}

	public void setHostName(String hostName) {
		this.hostName = hostName;
	}
	
	public void closeSockets()
	{
		if (messageSocket != null && !messageSocket.isClosed())
			try {
				messageSocket.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		
		if (dataSocket != null && !dataSocket.isClosed())
			try {
				dataSocket.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		
		if (dataServerSocket != null && !dataServerSocket.isClosed())
			try {
				dataServerSocket.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		
		if (localMessageServerSocket != null && !localMessageServerSocket.isClosed())
			try {
				localMessageServerSocket.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
	}

}
