/*
 * bcast.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 19 Jan 1996
 * $Id: bcast.c,v 1.1.1.1 2005/01/14 19:48:13 ferguson Exp $
 */
#include <stdio.h>
#include "trlib/send.h"
#include "util/debug.h"
#include "bcast.h"

/*
 * Functions defined here:
 */
void broadcast(char *content);

/*	-	-	-	-	-	-	-	-	*/

void
broadcast(char *content)
{
    static KQMLPerformative *perf;

    DEBUG1("broadcasting \"%s\"", content);
    if (perf == NULL) {
	if ((perf = KQMLNewPerformative("tell")) == NULL) {
	    return;
	}
    }
    KQMLSetParameter(perf, ":content", content);
    trlibSendPerformative(stdout, perf);
    DEBUG0("done");
}
