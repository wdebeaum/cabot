#!/bin/sh
#
# playraw.sh: Play raw pshinx audio files
#
# George Ferguson, ferguson@cs.rochester.edu, 27 Sep 2002
# $Id: playraw.sh,v 1.1 2007/09/17 12:38:10 ferguson Exp $
#
# Sphinx audio data is 16000Hz, signed word (16-bit) samples with
# no headers.
#

exec play -t raw -f s -s w -r 16000 "$@"
