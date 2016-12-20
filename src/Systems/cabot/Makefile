#
# File: Makefile
# Creator: George Ferguson
# Created: Wed Jun 20 15:12:58 2012
# Time-stamp: <Fri Nov 18 22:47:20 CST 2016 lgalescu>
#

NAME = trips-cabot

include ../Makefile-common

CSM_DATA = events goals models

SYS_DATA_DIR = $(etcdir)/cabot

install:: install-data

install-data:: BlockNames.tsv ${CSM_DATA} | ${SYS_DATA_DIR}
	$(INSTALL_DATA) $^ ${SYS_DATA_DIR}

${SYS_DATA_DIR}:
	mkdir -p $@
