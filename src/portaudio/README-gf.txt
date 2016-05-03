
File: README-gf.txt
Creator: George Ferguson
Created: Fri Feb 15 09:55:38 2013
Time-stamp: <Fri Feb 15 13:16:55 EST 2013 ferguson>

15 Feb 2013: portaudio for OSX 10.8.2

- See last line of this note: portaudio as it stands is deprecated

- src/common/pa_front.c says:
#define PA_VERSION_  1899
#define PA_VERSION_TEXT_ "PortAudio V19-devel (built " __DATE__  " " __TIME__ ")
  - this is the same as the latest from portaudio.org, FWIW

- For OSX 10.7 and up, PPC architecture is gone, hence need to
  configure with --enable-mac-universal=no

- Also, many of the functions used in portaudio are deprecated in
  10.8, so can't have the -Werror flag
  - except that configure.in contains, in the mac-specific section:
        CFLAGS="$CFLAGS -Werror"
    - so there's no way to override it
  - Could add -Wno-deprecated, expcept that gcc doesn't allow that
    flag for C!

- I decided to fix both of these by editing configure.in:
  - change default value of enable-mac-universal
  - remove -Werror from CFLAGS or add -Wno-error=deprecated
  - by themselves, these shouldn't affect anything

- However to make the changes take effect, I would need to recreate
  the configure script by running autoconf
  - it is very possible that a more modern autoconf will make a
    configure script that behaves differently on older platforms
    - the current configure script was created by autconf-2.61
    - autoconf-2.69 is current
  - this also remakes libtool (as it must) and portaudio-2.0.pc

- I saved the old configure script as "configure.old"
  - if the new one doesn't work for you, try the old one, possibly
    making the changes detailed above for OSX 10.7+

- Also Apple has deprecated most of the calls used by portaudio on
  OSX. That means the clock is running. But they also moved some
  definitions in their header files, specifically AudioDeviceID into
  CoreAudio/AudioHeardwareDeprecated.h, in case you didn't gethe the
  hint. I had to change src/include/pa_mac_core.h to pick these up. I
  tried to use a preprocesor incantation that only applies to
  10.8. YMMV.
