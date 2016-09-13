#
# File: Makefile
# Creator: George Ferguson
# Created: Wed Jun 20 15:12:58 2012
# Time-stamp: <Wed Aug  3 10:13:35 CDT 2016 lgalescu>
#

NAME = trips-cabot

include ../Makefile-common

CSM_DATA = events goals models

install:: install-data

install-data:: BlockNames.tsv ${CSM_DATA}
	$(INSTALL_DATA) $^ $(etcdir)
