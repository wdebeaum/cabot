
Time-stamp: <Mon Apr 20 15:51:09 CDT 2009 lgalescu>

19 Feb 2007

- Festival not compatible with recent versions of gcc. Solution is to
  explicitly invoke an older version:

  - On linux as of this date, that is:
        make CXX=g++32

  - On OSX, one couldp perhaps try:
        make CXX=g++-3.3

  Presumably this will eventually be a problem. Development of
  Festival seems to have stalled at 1.95 (pre-2.0).


<lgalescu>  Mon Apr 20 2009 15:44 CDT 

Some developments in SpeechOutMac have not been ported to SpeechOutFestival;
in particular, synchronization with SpeechIn, so the system doesn't listen to
itself. Hopefully I'll have time to combine the multiple SpeechOut* versions
into a single module and abstract away the messaging to minimize the need to
carry over updates from one module to another. Until then, if need arises for
un updated SpeechOutFestival, let me know.

--LG

