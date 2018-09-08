/*
 * @(#)SpeechController.java
 *
 * Dave Costello, costello@cs.rochester.edu, 19 Dec 1998
 * $Id: SpeechController.java,v 1.16 2018/08/31 22:37:28 lgalescu Exp $
 */

package TRIPS.SpeechController;

import java.io.IOException;
import java.util.Vector;
import java.util.StringTokenizer;
import java.util.NoSuchElementException;
import java.awt.Font;
import java.awt.Color;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.KeyEvent;
import javax.swing.BoxLayout;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.SwingConstants;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import TRIPS.TripsModule.StandardTripsModule;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLString;
import TRIPS.KQML.KQMLToken;
import TRIPS.util.GeometrySpec;
import TRIPS.util.StringUtils;
import TRIPS.util.JSizedButton;

public class SpeechController extends StandardTripsModule
				implements ActionListener, MouseListener {
    //
    // Fields
    //
    //constants
    protected static String CLICKMODE_MSG = "CLICK AND HOLD TO TALK";
    protected static String CONTINUOUS_MSG = "CONTINUOUS LISTENING MODE";
    protected static String EMPTY_SPEAKING_MSG = "            ";
    protected static String SPEAKING_MSG = "LISTENING...";
    protected static String SILENT_MSG = "SILENT";
    protected static String SYS_SPEAKING_MSG = "SPEAKING...";
    protected static String STARTED_MSG = "STARTED";
    protected static String NOT_STARTED_MSG = "NOT STARTED";
    protected static String DEFAULT_GEOMETRY = "25x4-0-0";
    protected static String NEWLINE = System.getProperty("line.separator");
    //instance variables
    protected String title = "Speech Recognition";
    protected boolean iconic = false;
    protected GeometrySpec geometry;
    protected int columns = 25;
    protected int rows = 4;
    protected int fontsize = 18;
    protected boolean showMenuBar = true;
    protected boolean showText = true;
    protected boolean showButton = true;
    protected String replaceSIL = "_";
    protected int stopDelay = 500;
    protected String sendTo = "speech-in";
    protected String listenTo = "speech-in"; 
    protected boolean continuousMode = true;
    protected boolean dynLMs = false;
    protected boolean isSpeaking = false;
    protected JFrame frame;
    protected JLabel listenToLabel;
    protected JLabel statusLabel;
    protected JLabel uttnumLabel;
    protected JLabel speakingLabel;
    protected JLabel signalLabel;
    protected JTextArea textArea;
    /* gf: no level meter
    protected JLevelMeter speechMeter;
    protected int meterLevels = 20;
    */
    protected JSizedButton talkButton;
    protected Vector inbuf = new Vector();
    JComboBox channelComboBox;
    String[] channelLabels = { "Phone", "Desktop" };
    String channelDefault = "Desktop";
    protected boolean showListeningTo = true;
    protected boolean showStatus = true;
    protected boolean showUttnum = true;
    protected boolean showChannel = true;

    //
    // Constructor
    //
    public SpeechController(String argv[], boolean isApplication) {
	super(argv, isApplication);
    }
    public SpeechController(String argv[]) {
	this(argv, false);
    }
    //
    // StandardTripsModule method
    //
    public void init() {
	if (name == null) {
	    // Unless overriden by parameters...
	    name = "speechcontroller";
	}
	// Perform standard initializations
	super.init();
	// Then parse our parameters
	handleParameters();
	// Create the widgets
	createWidgets();
	// We need to monitor status of speech modules
	try {
	    KQMLPerformative perf;
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (started-speaking . *) :sender " + sendTo + "))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (word . *) :sender " + sendTo + "))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (backto . *) :sender " + sendTo + "))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (stopped-speaking . *) :sender " + sendTo + "))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (utterance . *) :sender " + sendTo + "))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (module-status . *) :sender " + sendTo + "))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (sys-started-speaking . *)))");
	    send(perf);
	    perf = KQMLPerformative.fromString("(subscribe :content (tell &key :content (sys-stopped-speaking . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	// We need to set the recognizer mode (since it can't be set on cmdline)
	// pmc gone- will fix this later -gf
	//setContinuousMode(continuousMode);

	// We tell LMFactory whether we need dynamic LMs
	sendDynLMMode();

	// Tell the Facilitator we are ready
	ready();
    }
    // Parse parameters
    protected void handleParameters() {
	String value;
	if ((value = getParameter("-title")) != null) {
	    title = value;
	}
	if ((value = getParameter("-iconic")) != null) {
	    iconic = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-geometry")) != null) {
	    geometry = new GeometrySpec(value);
	} else {
	    geometry = new GeometrySpec(DEFAULT_GEOMETRY);
	}	    
	columns = geometry.width;
	rows = geometry.height;
	if ((value = getParameter("-columns")) != null) {
	    columns = Integer.parseInt(value);
	}
	if ((value = getParameter("-rows")) != null) {
	    rows = Integer.parseInt(value);
	}
	if ((value = getParameter("-fontsize")) != null) {
	    fontsize = Integer.parseInt(value);
	}
	if ((value = getParameter("-showMenuBar")) != null) {
	    showMenuBar = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-showText")) != null) {
	    showText = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-showButton")) != null) {
	    showButton = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-replaceSIL")) != null) {
	    replaceSIL = value;
	}
	if ((value = getParameter("-listenTo")) != null) {
	    listenTo = value;
	}
	if ((value = getParameter("-sendTo")) != null) {
	    sendTo = value;
	}
	if ((value = getParameter("-stopDelay")) != null) {
	    stopDelay = Integer.parseInt(value);
	}
	if ((value = getParameter("-mode")) != null) {
	    if (value.equalsIgnoreCase("ptt")) {
		continuousMode = false;
	    } else if (value.equalsIgnoreCase("continuous")) {
		continuousMode = true;
	    }
	}
	if ((value = getParameter("-dynLMs")) != null) {
	    dynLMs = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-channel")) != null) {
	    channelDefault = value;
	}
	if ((value = getParameter("-showListeningTo")) != null) {
	    showListeningTo = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-showStatus")) != null) {
	    showStatus = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-showUttnum")) != null) {
	    showUttnum = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-showChannel")) != null) {
	    showChannel = StringUtils.stringToBoolean(value);
	}
    }
    // Create the widgets for the applet
    protected void createWidgets() {
	JPanel panel, subpanel, subsubpanel;
	JLabel label;
	// Create JFrame
	frame = new JFrame();
	frame.setTitle(title);
	frame.setExtendedState(iconic ? JFrame.ICONIFIED : JFrame.NORMAL);
        // Frame layout
	Container contents = frame.getContentPane();
        contents.setLayout(new BorderLayout());
	// Status items
	panel = new JPanel();
	panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
	// Recognizer status
	subpanel = new JPanel();
	subpanel.setLayout(new BorderLayout());
	subsubpanel = new JPanel();
	label = new JLabel("Listening to: ");
	subsubpanel.add(label);
	listenToLabel = new JLabel(listenTo);
	subsubpanel.add(listenToLabel);
	if (showListeningTo) {
	    subpanel.add(subsubpanel, "West");
	}
	subsubpanel = new JPanel();
	label = new JLabel("Status: ");
	subsubpanel.add(label);
	statusLabel = new JLabel("Unknown");
	subsubpanel.add(statusLabel);
	if (showStatus) {
	    subpanel.add(subsubpanel, "East");
	}
	panel.add(subpanel);
	// Utterance info
	subpanel = new JPanel();
	subpanel.setLayout(new BorderLayout());
	subsubpanel = new JPanel();
	label = new JLabel("Uttnum: ");
	if (showUttnum) {
	    subsubpanel.add(label);
	}
	uttnumLabel = new JLabel(" - ");
	if (showUttnum) {
	    subsubpanel.add(uttnumLabel);
	}
	speakingLabel = new JLabel(continuousMode ? NOT_STARTED_MSG : EMPTY_SPEAKING_MSG);
	speakingLabel.setHorizontalAlignment(SwingConstants.CENTER);
	subpanel.add(speakingLabel, "Center");
	subpanel.add(subsubpanel, "West");
	signalLabel = new JLabel("          ");
	subpanel.add(signalLabel, "East");
	panel.add(subpanel, "South");
	// Info boxes:
	//subpanel = new JPanel();
	Box box = Box.createHorizontalBox ();
	box.add(Box.createHorizontalGlue());
	label = new JLabel("Channel:");
	if (showChannel) {
	    box.add(label);
	}
	channelComboBox = new JComboBox(channelLabels);
	channelComboBox.setEditable(true);
	channelComboBox.setSelectedItem(channelDefault);
	channelComboBox.setActionCommand("set-channel-info");
	channelComboBox.addActionListener(this);
	if (showChannel) {
	    box.add(channelComboBox);
	}
	/* gf: no level meter
	//+ level meter
	box.add(Box.createHorizontalGlue());
	JPanel levelMeter = new JPanel();
	JLabel meterLabel = new JLabel("Volume: ");
	levelMeter.add(meterLabel);
	speechMeter = new JLevelMeter(JLevelMeter.MeterLayout.HORIZONTAL, 
				      meterLevels, 20, 120);
	levelMeter.add(speechMeter);
	box.add(levelMeter);
	*/
	panel.add(box);
	contents.add(panel, BorderLayout.NORTH);
	// Text display
	if (showText) {
	    textArea = new JTextArea("", rows, columns);
	    textArea.setFont(new Font("SansSerif", Font.BOLD, fontsize));
	    textArea.setLineWrap(true);
	    textArea.setWrapStyleWord(true);
	    textArea.setEditable(false);
	    textArea.requestFocus();
	    contents.add(textArea, "Center");
	}
	// Talk Button
	if (showButton) {
	    talkButton = new JSizedButton(300, 65, continuousMode ? CONTINUOUS_MSG : CLICKMODE_MSG);
	    talkButton.addMouseListener(this);
	    if (showText) {
		contents.add(talkButton, "South");
	    } else {
		contents.add(talkButton, "Center");
	    }
	}
	// Possibly add menubar
        if (showMenuBar) {
	    createMenus();
	}
	// Pack the frame
	frame.pack();
	// Adjust location after packing has set size
	geometry.setLocationOfFrame(frame);
	// Show the frame
	frame.setVisible(true);
    }

    protected void createMenus() {
        // Create the menu bar
	JMenuBar menubar = new JMenuBar();
	JMenu menu;
	JMenuItem item;
	ButtonGroup group;
        // Control menu 
	menu = new JMenu("Control");
	// Kill Monitor
	item = new JMenuItem("Kill Speech Monitor");
	item.addActionListener(this);
	menu.add(item);
	// Separator
	menu.addSeparator();
	// Listening mode radio buttons
	group = new ButtonGroup();
	// Click to talk mode
	item = new JRadioButtonMenuItem("Click To Talk");
	item.addActionListener(this);
	item.setSelected(!continuousMode);
	group.add(item);
	menu.add(item);
	// Continuous mode
	item = new JRadioButtonMenuItem("Continuous");
	item.addActionListener(this);
	item.setSelected(continuousMode);
	group.add(item);
	menu.add(item);
	// Separator
	menu.addSeparator();
	// Dynamic LM updating
	item = new JCheckBoxMenuItem("Dynamic LMs");
	item.addActionListener(this);
	item.setSelected(dynLMs);
	menu.add(item);
	// Separator
	menu.addSeparator();
	// Parser mode radio buttons
	group = new ButtonGroup();
	// Online
	item = new JRadioButtonMenuItem("Parser Online");
	item.addActionListener(this);
	item.setSelected(true);
	group.add(item);
	menu.add(item);
	// Offline
	item = new JRadioButtonMenuItem("Parser Offline");
	item.addActionListener(this);
	item.setSelected(false);
	group.add(item);
	menu.add(item);
	// Separator
	menu.addSeparator();
	// Start-conversation
	item = new JMenuItem("Send START-CONVERSATION");
	item.addActionListener(this);
	menu.add(item);
	menubar.add(menu);
	// Separator
	menu.addSeparator();
	// Quit
	item = new JMenuItem("Quit", KeyEvent.VK_Q);
	item.addActionListener(this);
	menu.add(item);
	menubar.add(menu);
        // Font menu
	menu = new JMenu("Font");
	String fontnames[] = { "6", "8", "12", "14", "24", "36", "48" };
	int i;
	for (i=0; i < fontnames.length; i++) {
	    item = new JMenuItem(fontnames[i]);
	    item.addActionListener(this);
	    menu.add(item);
	}
	menubar.add(menu);
        // Help menu
	menu = new JMenu("Help");
	item = new JMenuItem("No help available");
	menu.add(item);
	// Not in JDK1.2...
	//menubar.setHelpMenu(menu);
	menubar.add(menu);
	// Add the menubar to the frame
	frame.setJMenuBar(menubar);
    }
    // Override StandardTripsModule method to cleanup GUI
    public void exit(int n) {
	if (frame != null) {
	    frame.dispose();
	}
	super.exit(n);
    }
    //
    // MouseListener methods
    //
    public void mouseClicked(MouseEvent e) {/*noop*/}
    public void mouseEntered(MouseEvent e) {/*noop*/}
    public void mouseExited(MouseEvent e)  {/*noop*/}
    public void mousePressed(MouseEvent e) {
	if (!continuousMode){
	    String channel = (String)channelComboBox.getSelectedItem();
	    KQMLList content = new KQMLList();
	    content.add("start-listening");
	    content.add(":channel");
	    content.add(channel);
	    send("request", "SPEECH-IN", content);
	}
    }
    public void mouseReleased(MouseEvent e){
	if (!continuousMode){
	    try {
		Thread.sleep(stopDelay);
	    } catch (Exception ex) {
		// Nothing to do
	    }
	    String channel = (String)channelComboBox.getSelectedItem();
	    KQMLList content = new KQMLList();
	    content.add("stop-listening");
	    content.add(":channel");
	    content.add(channel);
	    send("request", "SPEECH-IN", content);
	}
    }
    //
    // ActionListener methods
    //
    public void actionPerformed(ActionEvent evt) {
        String cmd = evt.getActionCommand();  
        if (cmd.equals("Quit")) {
	    exit(0);
	} else if (cmd.equals("Kill Speech Monitor")) {
	    send("request", "PM",
		 new KQMLList(new KQMLToken("kill"),
			      new KQMLToken("speechmon")));
	} else if (cmd.equals("Parser Online")) {
	    listenToLabel.setText(listenTo);
	    send("request", "PARSER", new KQMLList(new KQMLToken("online")));
	} else if (cmd.equals("Parser Offline")) {
	    listenToLabel.setText(listenTo + " (Parser offline)");
	    send("request", "PARSER", new KQMLList(new KQMLToken("offline")));
	} else if (cmd.equals("48") || cmd.equals("36") || cmd.equals("24") ||
		   cmd.equals("14") || cmd.equals("12") || cmd.equals("8") ||
		   cmd.equals("6")) {
	    int size = Integer.parseInt(cmd);
	    textArea.setFont(new Font("SansSerif", Font.BOLD, size));
	} else if (cmd.equals("Continuous")){
	    setContinuousMode(true);
	} else if (cmd.equals("Click To Talk")){
	    setContinuousMode(false);
	} else if (cmd.equals("Dynamic LMs")){
	    toggleDynLMsMode();
	} else if (cmd.equals("Send START-CONVERSATION")){
	    KQMLPerformative perf = new KQMLPerformative("broadcast");
	    KQMLPerformative content = null;
	    try {
		content = KQMLPerformative.fromString("(tell :content (start-conversation))");
	    } catch (IOException ex) {
		error("speechcontroller: couldn't parse start-conversation!");
		return;
	    }
	    perf.setParameter(":content", content);
	    send(perf);
	} else if (cmd.equals("set-channel-info")) {
	    String channel = (String)channelComboBox.getSelectedItem();
	    KQMLList content = new KQMLList();
	    content.add("set-channel-info");
	    content.add(":channel");
	    content.add(channel);
	    send("request", "SPEECH-IN", content);
	}
    }
    protected void toggleDynLMsMode() {
	dynLMs = !dynLMs;
	sendDynLMMode();
    }
    protected void sendDynLMMode() {
	KQMLList content = new KQMLList();
	content.add(new KQMLToken("set-dynlm-mode"));
	content.add(new KQMLToken(dynLMs ? "true" : "false"));
	send("request", content);
    }
    protected void setContinuousMode(boolean flag) {
	continuousMode = flag;
	KQMLList content = new KQMLList();
	content.add(new KQMLToken("set-mode"));
	content.add(new KQMLToken(flag ? "continuous" : "ptt"));
	send("request", "SPEECH-IN", content);
	talkButton.setText(flag ? CONTINUOUS_MSG : CLICKMODE_MSG);
	talkButton.setEnabled(!flag);
	// I think this is the right thing to do...
	setIsSpeaking(false);
    }
    protected void setIsSpeaking(boolean flag) {
	isSpeaking = flag;
	if (continuousMode) {
	    speakingLabel.setText(flag ? SPEAKING_MSG : SILENT_MSG);
	} else {
	    speakingLabel.setText(EMPTY_SPEAKING_MSG);
	}
    }
    //---------------------------------------------------------------
    //
    // KQMLReceiver methods overriding StandardTripsApplet defaults
    //
    public void receiveTell(KQMLPerformative msg, Object contentobj) {
	if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, "content was not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	String content0 = content.nth(0).toString();
	if (content0.equalsIgnoreCase("started-speaking")) {
	    receiveTellStartedSpeaking(msg, content);
        } else if (content0.equalsIgnoreCase("stopped-speaking")) {
	    receiveTellStoppedSpeaking(msg, content);
	} else if (content0.equalsIgnoreCase("word")) {
	    receiveTellWord(msg, content);
	} else if (content0.equalsIgnoreCase("backto")) {
	    receiveTellBackto(msg, content);
	} else if (content0.equalsIgnoreCase("utterance")) {
	    receiveTellUtterance(msg, content);    
	} else if (content0.equalsIgnoreCase("module-status")) {
	    receiveTellModuleStatus(msg, content);    
	} else if (content0.equalsIgnoreCase("start-conversation")) {
	    receiveTellStartConversation();
	} else if (content0.equalsIgnoreCase("end-conversation")) {
	    receiveTellEndConversation();
	} else if (content0.equalsIgnoreCase("new-scenario") ||
		   content0.equalsIgnoreCase("modify-scenario")) {
	    // Ignore
	} else if (content0.equalsIgnoreCase("sys-started-speaking")) {
	    receiveTellSysStartedSpeaking();
	} else if (content0.equalsIgnoreCase("sys-stopped-speaking")) {
	    receiveTellSysStoppedSpeaking();
	} else {
	    errorReply(msg, "bad tell: " + content0);
	}
    }
    protected void receiveTellStartConversation() {
	reset();
	showWindow();
	// Recognizer starts on start-conversation in continuous mode
	if (continuousMode) {
		speakingLabel.setText(STARTED_MSG);
	}
    }
    protected void receiveTellEndConversation() {
	hideWindow();
    }
    protected void receiveTellSysStartedSpeaking() {
	speakingLabel.setText(SYS_SPEAKING_MSG);
	KQMLList content = new KQMLList();
	content.add("interrupt-listening");
	send("request", "SPEECH-IN", content);
    }
    protected void receiveTellSysStoppedSpeaking() {
	speakingLabel.setText(SILENT_MSG);
	KQMLList content = new KQMLList();
	content.add("resume-listening");
	send("request", "SPEECH-IN", content);
    }
    protected void receiveTellStartedSpeaking(KQMLPerformative msg, KQMLList content) {
	KQMLObject uttnumobj = content.getKeywordArg(":uttnum");
	if (uttnumobj == null) {
	    errorReply(msg, "missing :uttnum");
	    return;
	}
	int uttnum = Integer.parseInt(uttnumobj.toString());
	uttStart(uttnum);
	setIsSpeaking(true);
    }
    protected void receiveTellStoppedSpeaking(KQMLPerformative msg, KQMLList content) {
	setIsSpeaking(false);
    }
    protected void receiveTellWord(KQMLPerformative msg, KQMLList content) {
	if (content.length() < 2) {
	    errorReply(msg, "missing word in :content");
	    return;
	}
	/* gf: no level meter
	// update meter
        KQMLToken peak = (KQMLToken) content.getKeywordArg(":peak");
	if (peak != null) {
	    speechMeter.setLevel((int) Math.ceil(meterLevels * Integer.parseInt(peak.toString()) / 32768));  
	}
	*/
	// Extract word (should be string; possibly token)
	KQMLObject kword = content.nth(1);
	String word;
	if (kword instanceof KQMLString) {
	    word = ((KQMLString)kword).stringValue();
	} else {
	    word = kword.toString();
	}
	// Let's be flexible about what we take for :frame
	Object frameobj = content.getKeywordArg(":frame");
	if (frameobj == null) {
	    errorReply(msg, "missing :frame");
	    return;
	}
	int start;
	int end;
	if (frameobj instanceof KQMLList) {
	    KQMLList framelist = (KQMLList)frameobj;
	    if (framelist.length() == 1) {
		start = Integer.parseInt(framelist.nth(0).toString());
		end = start;
	    } else if (framelist.length() == 2) {
		start = Integer.parseInt(framelist.nth(0).toString());
		end = Integer.parseInt(framelist.nth(1).toString());
	    } else {
		errorReply(msg, "bad list in :frame");
		return;
	    }
	} else {
	    start = Integer.parseInt((String)frameobj);
	    end = start;
	}
	uttWord(word, start, end);
    }
    protected void receiveTellBackto(KQMLPerformative msg, KQMLList content) {
	KQMLObject frameobj = content.getKeywordArg(":frame");
	if (frameobj == null) {
	    errorReply(msg, "missing :frame");
	    return;
	}
	int frame = Integer.parseInt(frameobj.toString());
	uttBackto(frame);
    }
    protected void receiveTellUtterance(KQMLPerformative msg, KQMLList content) {
	KQMLObject textobj = content.getKeywordArg(":text");
	if (textobj == null) {
	    errorReply(msg, "missing :text in end");
	    return;
	}
	String text = ((KQMLString)textobj).stringValue();
	KQMLObject signalobj = content.getKeywordArg(":signal");
	String signal = (signalobj != null) ? signalobj.toString() : "";
	uttEnd(text, signal);
	/* gf: no level meter
	// reset meter; but wait a little while
	try {
	    Thread.sleep(200);
	} catch (Exception x) {}
	speechMeter.reset();  
	*/
    }
    public void receiveTellModuleStatus(KQMLPerformative msg, KQMLList content) {
	String who = null;
	if (content.length() == 3) {
	    who = content.nth(1).toString();
	} else if (content.length() == 2) {
	    KQMLObject senderobj = msg.getParameter(":sender");
	    if (senderobj == null) {
		errorReply(msg, "content length == 2 and no :sender");
		return;
	    }
	    who = senderobj.toString();
	} else {
	    errorReply(msg, "content length not 2 or 3");
	}
	if (who.equalsIgnoreCase(listenTo)) {
	    String status = content.nth(content.length()-1).toString();
	    Color color;
	    if (status.equalsIgnoreCase("ready")) {
		color = Color.green;
	    } else if (status.equalsIgnoreCase("connected")) {
		color = Color.yellow;
	    } else {
		color = Color.red;
	    }
	    statusLabel.setText(status.toUpperCase());
	    //statusLabel.setForeground(color);
	}
    }
    public void receiveRequest(KQMLPerformative msg, Object contentobj) {
	if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, "content was not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	String content0 = content.nth(0).toString();
	if (content0.equalsIgnoreCase("exit")) {
	    int status = 0;
	    if (content.length() > 1) {
		status = Integer.parseInt(content.nth(1).toString());
	    }
 	    exit(status);
	} else if (content0.equalsIgnoreCase("hide-window")) {
	    hideWindow();
	} else if (content0.equalsIgnoreCase("show-window")) {
	    showWindow();
	} else if (content0.equalsIgnoreCase("chdir")) {
	    // Ignore
	} else if (content0.equalsIgnoreCase("reset")) {
	    reset();
	} else if (content0.equalsIgnoreCase("grab")) {
	    System.err.println("SpeechController: grab request not implemented");
	} else if (content0.equalsIgnoreCase("ungrab")) {
	    System.err.println("SpeechController: ungrab request not implemented");
	} else {
	    errorReply(msg, "bad request: " + content0);
	}
    }
    // KQML output
    protected void send(String verb, String receiver, KQMLObject content) {
	KQMLPerformative perf = new KQMLPerformative(verb);
	perf.setParameter(":receiver", receiver);
	perf.setParameter(":content", content);
	send(perf);
    }
    protected void send(String verb, KQMLObject content) {
	KQMLPerformative perf = new KQMLPerformative(verb);
	perf.setParameter(":content", content);
	send(perf);
    }
    // Make window visible
    public void showWindow(){
	frame.setExtendedState(JFrame.NORMAL);
    }
    // Make window invisible
    public void hideWindow(){
	frame.setExtendedState(JFrame.ICONIFIED);
    }
    // Clear text area
    public void reset() {
	textArea.setText("");
    }
    // Word buffer methods
    protected void uttStart(int uttnum) {
	displayUttnum(uttnum);
	inbuf.removeAllElements();
	updateTextArea();
    }
    protected void uttWord(String word, int start, int end) {
	inbuf.addElement(new SpeechToken(word, start, end));
	updateTextArea();
    }
    protected void uttBackto(int frame) {
	for (int i=inbuf.size()-1; i >= 0; i--) {
	    SpeechToken token = (SpeechToken)inbuf.elementAt(i);
	    if (token.start >= frame) {
		inbuf.removeElementAt(i);
	    }
	}
	updateTextArea();
    }
    protected void uttEnd(String text, String signal) {
	displayText(text);
	if (signal == null || signal.equals("")) {
	    displaySignal("");
	} else if (signal.equalsIgnoreCase("too-loud")) {
	    displaySignal("Too Loud!");
	} else if (signal.equalsIgnoreCase("too-soft")) {
	    displaySignal("Too Soft!");
	} else {
	    warn("uttEnd: unknown :signal value: " + signal);
	}
    }
    //
    // Display routines
    //
    protected void displayUttnum(int uttnum) {
	if (uttnum <= 0) {
	    uttnumLabel.setText(" - ");
	} else {
	    uttnumLabel.setText(Integer.toString(uttnum));
	}
    }
    protected void updateTextArea() {
	textArea.setText("");
	for (int i=0; i < inbuf.size(); i++) {
	    SpeechToken token = (SpeechToken)inbuf.elementAt(i);
	    String word = token.word;
	    if (word.equalsIgnoreCase("<SIL>")) {
		word = replaceSIL;
	    }
	    displayWord(word);
	    textArea.append(" ");
	}    
	// Scroll to end
	textArea.setCaretPosition(textArea.getDocument().getLength());
    }
    protected void displayText(String text) {
	textArea.setText("");
	try {
	    StringTokenizer tokens = new StringTokenizer(text);
	    while (tokens.hasMoreTokens()) {
		String word = tokens.nextToken();
		if (word.equalsIgnoreCase("<SIL>")) {
		    word = replaceSIL;
		}
		displayWord(word);
		textArea.append(" ");
	    }
	    // Scroll to end
	    textArea.setCaretPosition(textArea.getDocument().getLength());
	} catch (NoSuchElementException ex) {
	}
    } 
    protected void displayWord(String word) {
	textArea.append(word);
    }
    protected void displaySignal(String str) {
	signalLabel.setText(str);
    }
    //
    // When run as an application
    //
    public static void main(String argv[]) {
	new SpeechController(argv, true).run();
    }
}
