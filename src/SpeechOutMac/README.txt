
README for SpeechOutMac

gf: 4 Jan 2010:

- The Carbon APIs on which our use of SpeechManager is based have been
  deprecated for some time. Under OSX 10.6 (Snow Leopard) they have
  finally disappeared. Interestingly, the actual SpeechManager APIs
  are still supported, but I ran into a missing function,
  GetDateTime(), used in pronunciation.c. Commenting out that function
  allowed the program to work again, but  obviously at some point we
  need to move to the Cocoa API for speech synthesis (NSSpeechSynthesizer).

- The programs mactts.m and listvoices.m are Objective-C code that
  does the same thing as the corresponding original C programs (the
  ".c" versions), but using the NSSpeechSynthesizer class. These work
  as expected, which was a nice surprise.

- Unfortunately, my quick attempt to work this through ran into a
  problem implementing the "delegate" (or callback) methods (in
  mactts2.m). This is needed at the very least to get the
  "didFinishSpeaking" notification in order to send a DONE message
  (without a busy wait on the synthesizer's status). I'm guessing that
  these are dispatched using the standard Cocoa event dispatcher,
  meaning that they aren't dispatched at all in our simple program
  that invokes the synthesizer in main(). I did some looking, and
  there are ways to do one's own dispatching, but it's complicated and
  multi-threaded and would need to integrated with however we plan to
  handle the TRIPS messaging in a Cocoa app (which is also effectively
  multi-threaded). For future reference, here's a pointer:
    http://cocoawithlove.com/2009/01/demystifying-nsapplication-by.html


---------------------------------------------------------------------------

Sample programs:

  - mactts.c: A sample program from somewhere that shows how to use
              the TTS APIs.

  - listvoices.c: A piece of mactts.c useful for listing available
                  available voices (the "-printvoices" option from
                  mactts).

URL references:

  Chapter 4 - Speech Manager
    http://developer.apple.com/documentation/mac/Sound/Sound-187.html

  Using the Speech Manager
    http://developer.apple.com/documentation/mac/Sound/Sound-193.html

  Using Embedded Speech Commands
    http://developer.apple.com/documentation/mac/Sound/Sound-200.html

  Phonemic Representation of Speech
    http://developer.apple.com/documentation/mac/Sound/Sound-201.html

  The Pronunciation Dictionary Resource
    http://developer.apple.com/documentation/mac/Sound/Sound-264.html

Pointers:

  /System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/CarbonCore.framework/Versions/A/Headers

  /Developer/Examples/Speech/SpeechSynthesisExamples/CocoaSpeechSynthesisExample/Sources/English.lproj/SpeakingTextWindow.m

List of voices as of 14 Dec 2004 on my OSX 10.3.6 system:

Agnes      f   35 Isn't it nice to have a computer that will talk to you?
Albert     m   30 I have a frog in my throat. No, I mean a real frog!
Bad News   m   50 The light you see at the end of the tunnel is the headlamp of a fast approaching train.
Bahh       n    2 Do not pull the wool over my eyes.
Bells      n  100 Time flies when you are having fun.
Boing      n    1 Spring has sprung, fall has fell, winter's here and it's colder than usual.
Bruce      m   35 I sure like being inside this fancy computer
Bubbles    n    0 Pull the plug! I'm drowning!
Cellos     n   50 Doo da doo da dum dee dee doodly doo dum dum dum doo da doo da doo da doo da doo da doo da doo
Deranged   m   30 I need to go on a really long vacation.
Fred       m   30 I sure like being inside this fancy computer
Hysterical m   30 Please stop tickling me!
Junior     m    8 My favorite food is pizza.
Kathy      f   30 Isn't it nice to have a computer that will talk to you?
Pipe Organ n  500 We must rejoice in this morbid voice.
Princess   f    8 When I grow up I'm going to be a scientist.
Ralph      m   50 The sum of the squares of the legs of a right triangle is equal to the square of the hypotenuse.
Trinoids   n 2001 We cannot communicate with these carbon units.
Vicki      f   35 Isn't it nice to have a computer that will talk to you?
Victoria   f   35 Isn't it nice to have a computer that will talk to you?
Whisper    m   30 Pssssst, hey you, Yeah you, Who do ya think I'm talking to, the mouse?
Zarvox     n    1 That looks like a peaceful planet.

Victoria and Vicki are the best female voices. Bruce is the best male
voice.

Thu Aug  2 10:19:22 2018
<lgalescu>
Update on Mac voices

Currently, the best female voice available seems to be Ava (US-En). Other good
voices are Fiona (Scot-En), Karen (AU-En, the enhanced version). Samantha
(US-En), Susan (US-En) and Serena (UK-En) are also reasonably good.

There are fewer good male voices. Daniel (UK-En) is probably the best. Lee
(AU-En) is good, too. Tom is the best US-En voice, but not as good as the
others.

***

Of note: the code still works in MacOS 10.12 (Sierra), even though more
functions are now deprecated. There's a good chance, however, that these
functions will be completely removed in future MacOS updates.
