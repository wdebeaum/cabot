#
# File: Systems/bob/Makefile
#

NAME = trips-bob

include ../Makefile-common

$(IMAGE):: ../../TextTagger/drum-dsl-resources.lisp ../../TextTagger/drum-mappings.lisp

CSM_DATA = events goals models goals-par

SYS_DATA_DIR = $(etcdir)/bob

install:: install-configs install-data

install-configs:: TextTagger-*.conf
	$(INSTALL_DATA) $^ $(etcdir)

install-data:: ${CSM_DATA} | ${SYS_DATA_DIR}
	$(INSTALL_DATA) $^ ${SYS_DATA_DIR}

${SYS_DATA_DIR}:
	mkdir -p $@
