/*
 * ad_pa_net.c: Read audio data from pa_net_server.
 *
 * George Ferguson, ferguson@cs.rochester.edu, 31 Mar 2006
 * Time-stamp: <Wed Apr  5 16:02:40 EDT 2006 ferguson>
 *
 * See ../portaudio_net/README.txt for more information on the client-server
 * audio model.
 *
 * Derived very loosely from ad_pa.c.
 *
 * Uses value of TRIPS_AUDIO_SERVER in format hostname:cport:dport
 * if set, else localhost and default port numbers.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> 
#include <arpa/inet.h> /* inet_ntoa */

#include "s3types.h"
#include "ad.h"
#include "pa_net.h"

/** Sockets used for server connection */
static int controlS, dataS;

/**
 * Open and return a socket connection to the given hostname/port.
 */
static int
openSocket(char *hostname, int port)
{
    int sock;
    struct sockaddr_in addr;
    struct hostent *host;

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
	perror("socket");
	return -1;
    }
    host = gethostbyname(hostname);
    if (host == NULL) {
        fprintf(stderr, "%s: host not found", hostname);
	return -1;
    }
    bzero((char *)&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    bcopy((char *)host->h_addr, (char *)&addr.sin_addr.s_addr, host->h_length);
    addr.sin_port = htons(port);
    fprintf(stderr, "sphinx3: ad_pa_net: connecting to %s:%d (%s)\n", hostname, port, inet_ntoa(addr.sin_addr));
    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
	perror("connect");
	return -1;
    }
    fprintf(stderr, "sphinx3: ad_pa_net: connected to %s:%d (%s)\n", hostname, port, inet_ntoa(addr.sin_addr));
    return sock;
}

/**
 * Send the given cmd over the control connection.
 */
static void
sendCmd(char *cmd)
{
    write(controlS, cmd, strlen(cmd));
    write(controlS, "\n", 1);
    fprintf(stderr, "sphinx3: ad_pa_net: sending cmd: %s\n", cmd);
}

/**
 * NOTE: Samples per sec is fixed in server (for now).
 */
ad_rec_t *ad_open_sps (int32 samples_per_sec)
{
    ad_rec_t *handle;
    char *s;
    char hostname[1024];
    int cport = PA_NET_DEFAULT_CONTROL_PORT;
    int dport = PA_NET_DEFAULT_DATA_PORT;

    /* Get server info from environment (if any) */
    strcpy(hostname, "localhost");
    if ((s = getenv("TRIPS_AUDIO_SERVER")) != NULL) {
	fprintf(stderr, "sphinx3: ad_pa_net: TRIPS_AUDIO_SERVER=%s\n", s);
	if (sscanf(s, "%1023[^:]:%d:%d", hostname, &cport, &dport) != 3) {
	    fprintf(stderr, "bad format for TRIPS_AUDIO_SERVER: %s\n", s);
	    abort();
	}
    }
    /* Open control connection */
    controlS = openSocket(hostname, cport);
    if (controlS < 0) {
	abort();
    }
    fprintf(stderr, "sphinx3: ad_pa_net: control connection to %s:%d: %d\n", hostname, cport, controlS);
    /* Open data connection */
    dataS = openSocket(hostname, dport);
    if (dataS < 0) {
	abort();
    }
    fprintf(stderr, "sphinx3: ad_pa_net: data connection to %s:%d: %d\n", hostname, dport, dataS);
    fcntl(dataS, F_SETFL, O_NONBLOCK);

    /* Allocate and initialize ad_rec */
    if ((handle = (ad_rec_t *) calloc (1, sizeof(ad_rec_t))) == NULL) {
	fprintf(stderr, "calloc(%ld) failed\n", sizeof(ad_rec_t));
	abort();
    }
    handle->sps = samples_per_sec;
    handle->bps = AD_SAMPLE_SIZE;
    handle->recording = 0;

    /* Return ad_rec */
    return handle;
}

ad_rec_t *ad_open ( void )
{
  return ad_open_sps(DEFAULT_SAMPLES_PER_SEC);
}

int32 ad_start_rec (ad_rec_t *handle)
{
    if (handle->recording)
	return AD_ERR_GEN;

    handle->recording = 1;

    sendCmd("start");
    return 0;
}

int32 ad_stop_rec (ad_rec_t *handle)
{
    if (! handle->recording)
	return AD_ERR_GEN;

    sendCmd("stop");

    handle->recording = 0;
    return 0;
}

int32 ad_read (ad_rec_t *handle, int16 *buf, int32 max)
{
    int maxBytesToRead = max * handle->bps;
    fprintf(stderr, "sphinx3: ad_pa_net: reading max=%d samples (%d bytes)\n", max, maxBytesToRead);
    int numBytesRead = read(dataS, buf, maxBytesToRead);
    int numSamplesRead = numBytesRead / handle->bps;
    if ((numSamplesRead == 0) && (!handle->recording)) {
	fprintf(stderr, "sphinx3: ad_pa_net: EOF\n");
	return AD_EOF;
    }
    fprintf(stderr, "sphinx3: ad_pa_net: read %d samples\n", numSamplesRead);
    return numSamplesRead;
}

int32 ad_close (ad_rec_t *handle)
{
    if (handle->recording) {
	if (ad_stop_rec(handle) < 0) {
	    return AD_ERR_GEN;
	}
    }

    close(dataS);
    close(controlS);

    free(handle);

    return 0;
}
