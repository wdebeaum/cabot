/*
 * log.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 26 Sep 2002
 * $Id: log.c,v 1.1.1.1 2005/01/14 19:48:13 ferguson Exp $
 */
#include <stdio.h>
#include <sys/param.h>
#include "log.h"
#include "utt.h"
#include "main.h"
#include "util/memory.h"
#include "util/error.h"

FILE *logfp;

char *logdir = NULL;

void
logChdir(char *dir)
{
    FILE *fp;
    char filename[MAXPATHLEN];

    /* Free old logdir and save new one */
    gfree(logdir);
    logdir = gnewstr(dir);
#ifdef undef
    /* Open new log file */
    if (dir) {
		sprintf(filename, "%s/sphinx.log", dir);
    } else {
		strcpy(filename, "sphinx.log");
    }
    if ((fp=fopen(filename, "w")) == NULL) {
		/* Error, oh well */
		SYSERR1("couldn't write %s", filename);
		return;
    } else {
		/* Open ok, close old logfile */
		close(logfp);
		logfp = fp;
		/* Write info about sphinx to new logfile */
		dumpSphinxInfo(logfp);
		fflush(logfp);
    }
#endif
}
