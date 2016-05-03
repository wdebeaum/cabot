/*
 * analyze.c: Analyze sphinx speech input file for signal-level problems
 *
 * George Ferguson, ferguson@cs.rochester.edu,  2 Apr 1998
 * $Id: analyze.c,v 1.2 2008/05/22 17:40:33 lgalescu Exp $
 *
 * History:
 * 2008/05/22 L Galescu - Finer control; fixed bug.
 */
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include "analyze.h"

#define BUFFER_SIZE 2048
#define SIGNIFICANT_BITS 5
#define SHIFT_FACTOR (15-SIGNIFICANT_BITS)
#define NUM_BUCKETS (1<<SIGNIFICANT_BITS)

int
analyze(char *filename)
{
    int fd;
    short samples[BUFFER_SIZE], sample;
    int nread,nsamples;
    int count[NUM_BUCKETS];
    int i;

    /* Open file */
    if ((fd = open(filename, O_RDONLY)) < 0) {
	perror(filename);
	return 0;
    }
    /* Clear buckets */
    for (i=0; i < NUM_BUCKETS; i++) {
	count[i] = 0;
    }

    /* Read samples to EOF */
    while ((nread = read(fd, (char*)samples, sizeof(samples))) > 0) {
	nsamples = nread / sizeof(short);
	/* Scan this batch of samples */
	for (i=0; i < nsamples; i++) {
	    sample = samples[i];
	    /* lg 2008/05/22: bug fix: -(-32768) = -32768 !! */
	    if (sample == (short) -32768) {
	      sample++;
	    }
	    /* Take absolute value */
	    if (sample < 0) {
		sample = -sample;
	    }
	    /* Store in bucket */
	    count[sample >> SHIFT_FACTOR] += 1;
	}
    }
    /* Close file when done */
    close(fd);
    /* If we have more than a few samples in the top bucket */
    if (count[NUM_BUCKETS-1] > 20) {
	/* Then there's probably clipping */
	return 1;
    } else if (count[(NUM_BUCKETS >> 3) - 1] < 40) {
	/* Very few samples in bucket 4 means too soft */
	return -1;
    } else {
	/* Otherwise ok */
	return 0;
    }
}
