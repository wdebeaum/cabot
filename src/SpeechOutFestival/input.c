/*
 * input.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * $Id: input.c,v 1.1.1.1 2005/01/14 19:48:14 ferguson Exp $
 */
#include <stdio.h>
#include "trlib/input.h"
#include "recv.h"
#include "input.h"
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
