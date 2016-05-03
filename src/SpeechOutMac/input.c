/*
 * input.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/03/08 17:55:22 ferguson>
 */
#include <stdio.h>
#include "trlib/input.h"
#include "recv.h"
#include "main.h"

/*
 * Functions defined here:
 */
void input(int fd);

/*	-	-	-	-	-	-	-	-	*/

void
input(int fd)
{
    int ret;

    if ((ret = trlibInput(fd, TRLIB_BLOCK, receiveMsg)) <= 0) {
	programExit(ret);
    }
}
