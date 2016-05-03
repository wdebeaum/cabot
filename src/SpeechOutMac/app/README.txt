
gf: 11 Jan 2010

First attempt at a Cocoa version of the SpeechOutMac application. Unlike the
original SpechOutMac, which was faceless, this one has a UI with which you
can test the speech (and sound) output. This is often useful when setting up
anyway. And since it's more trouble to make a faceless Cocoa app anyway, you
get a UI (which you can always hide).

The following classes are involved:

- KQMLStream: A class that wraps the C version of the KQML library around a Cocoa
  socket stream for use connecting to the facilitator.
  
- TripsAgent: A class that provides the basic message handling capability on top
  of a KQMLStream.

- SpeechOutMacAppDelegate: The `main' body of the Cocoa app, connected to the UI
  via MinaMenu.xib (in Interface Builder). These is also where we handle the
  specific messages that this agent needs to understand (such as `say' in this
  case).

- MySynthesizer: A class that wraps NSSpeechSynthesizer so that we can use methods
  rather than property get/set calls.
  
- VoicesPopUpButton: A special control for selecting the speech synthesis voice.

Notes:

- This code requires OSX 10.6 and XCode 3.2. I tried setting the "General" info of
  the overall project to use the 10.5 SDK, but it appears that NSStreamDelegate and
  NSSpeechSynthesizerDelegate don't exist in that version. These things could
  perhaps be worked around...

- Facilitator connection is still a bit ad hoc
  - Hardcoded to localhost:6200
  - If we fail to connect, we don't get notified until after we first try to write.
	How is that a good idea?
	- Thinking about redoing this NSStream busines with NSSocketPort instead...
    - No, that isn't right. There are some examples floating about, like Apple's
	  CocoaEcho demo, which uses NSNetService:getInputStream:outputStream, but then
	  it's the same as NSOutputStream, so probably has the same delay reporting the
	  error.
	- This "MyNetwork" example is probably good, but overkill for current purposes
	  from the client end:
	    http://mooseyard.lighthouseapp.com/projects/23191-mynetwork
	- Turns out that trying to wqrite a 0-length string triggers the error, so I
	  added that to the KQMLStream initialization code. Check that. Writing the
	  empty string causes a failure if the server really is there. So now I send
	  a newline instead, whihc I know the Facilitator will ignore (YMMV for other
	  network services).
	
- The magic of handling preferences is done with Cocoa Bindings and the UserDefaults controller.
  This is all setup in InterfaceBuilder. Such magic can make it hard for someone else to
  figure out what's going on from the code...

- When there's no preferences file for the app, an empty menu item is generated in
  the popup menu on the Preferences panel.
  - I think this is a bad interaction between the Cocoa bindings that manage (save,
    restore, and update) these settings (including the choice of voice), and the fact
	that the voices menu is generated programmatically (VoicesPopUpMenu.m).
  - I tried specifying "Use System Default" as the "Null Placeholder" in InterfaceBuilder,
    but that just resulted in two menu items with that name.
  - I tried setting default preferences as described here:
      http://developer.apple.com/mac/library/documentation/Cocoa/Conceptual/CocoaBindings/Concepts/NSUserDefaultsController.html
	  Section "initialValues Versus NSUserDefaults registerDefaults:"
	but that did nothing.
  - I hacked around this by creating openPrefsPane and just fixing the menu if it's
	broken. Ugly but it works. And it lets me call updateVoiceParams, which needed
	to happen anyway to make sure the sliders show the values for the voice restored
	from the defaults (which doesn't invoke didSelectVoice).
	- Correction: I need to call updateSpeechParams in the initialization code anyway
	  since you might hit Speak without opening the prefs!
	
- I get three errors in the console whenever the voice is changed for the first time:
    NSSpeechSynthesizer: [MySpeechSynthesizer _setObject:forProperty:usingDataSize:] - Error -50
  - This must be coming from the NSSpeechSynthesizer method setVoice.
  - Perhaps because the voice selected in System Prefs is Ceptral Diane?

- Changing any of the parameters from their default values generates errors in the console
  - Seems like the property handling is pretty bad in NSSpeechSynthesizer
    - Docs are bogus:
	    http://www.openradar.appspot.com/6928128
	  - Note the rate and pitch base properties have the same documentation, but the
	    definitive reference cited in the OpenRadar post says what they should be:
		  http://developer.apple.com/documentation/mac/Sound/Sound-190.html#MARKER-2-95
	- OpenRadar has another entry (although it is claimed that is all fixed in 10.6):
	    http://openradar.appspot.com/6524554
	- I'm guessing from the error message that the NSNumber* we pass in doesn't get
	  turned into the fixed or floating point value needed by the underlying Speech
	  Manager API (which is presumbaly still there and in fact does work).

- Switching between Apple and Cepstral voices causes the Apple voices to "misbehave"
  until the app is quit and restarted.
  - I assume this is the same problem referred on the Cepstral website:
      http://cepstral.com/cgi-bin/version_history?vid=1261747057&pid=1261170303
	"Known issue: switching from a Cepstral voice to an Apple voice in the Speech
	Preference Pane may cause Apple Voices to misbehave until the System Preferences
	Application is relaunched."

- In XCode: Search path to find KQML code:
  - Target > Info > Build > Header Search Paths: ../..
  - For linking...
    - We could setup some paths in the Build settings
	- Or we could pull in the files directly
	- Hmmm
	- I added KQLMStream.[hm] to "Other Sources" (so they get compiled with this project)
	  and I added libKQML to "Frameworks" > "Other Frameworks" (it just gets linked). This
	  seems to work. Have to check that the paths are relative after a checkout.
	- I also needed to add a definition of the variable `progname' to main.m and initialize
	  it since that variable is used by many of the TRIPS C library routines


