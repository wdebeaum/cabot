/*
 * main.cpp
 *
 * George Ferguson, ferguson@cs.rochester.edu,  9 Oct 2003
 * Time-stamp: <Wed Feb 21 12:36:28 EST 2007 ferguson>
 */

#include "festival.h"
extern "C" {
#include "input.h"
#include "send.h"
#include "main.h"
}

void parseArgs(int argc, char **argv);
void usage();
void argerr(char *fmt, char *s);

FILE *debugfp;
char *progname;
char *voice_cmd, *eval_cmd;

int
main(int argc, char **argv) {
    int heap_size = 210000;  // default scheme heap size
    int load_init_files = 1; // we want the festival init files loaded

    progname = argv[0];

    parseArgs(argc, argv);

    festival_initialize(load_init_files, heap_size);

    if (voice_cmd != NULL) {
	festival_eval_command(voice_cmd);
    }
    if (eval_cmd != NULL) {
	festival_eval_command(eval_cmd);
    }

    sendReadyMsg();
    while(1) {
	input(0);
    }
    /* NOTREACHED */
    return 0;
}

void
parseArgs(int argc, char **argv) {
    argc -= 1;
    argv += 1;
    while (argc > 0) {
	if (!argv[0]) {
	    break;
	}
	if (streq("--help", argv[0])) {
	    usage();
	}
	if (!argv[1]) {
	    break;
	}
	if (streq("--libdir", argv[0])) {
	    festival_libdir = argv[1];
	} else if (streq("--voice", argv[0])) {
	    if (streq(argv[1], "ked")) {		/* Standard voices */

		voice_cmd = "(voice_ked_diphone)";
	    } else if (streq(argv[1], "kal")) {
		voice_cmd = "(voice_kal_diphone)";
	    } else if (streq(argv[1], "rab")) {		/* Other CSTR voices */
		voice_cmd = "(voice_rab_diphone)";
	    } else if (streq(argv[1], "el")) {
		voice_cmd = "(voice_el_diphone)";
	    } else if (streq(argv[1], "as")) {		/* OGI voices */
		voice_cmd = "(voice_as_diphone)";
	    } else if (streq(argv[1], "mwm5")) {
		voice_cmd = "(voice_mwm5_diphone)";
	    } else if (streq(argv[1], "aec")) {
		voice_cmd = "(voice_aec_diphone)";
	    } else if (streq(argv[1], "jph")) {
		voice_cmd = "(voice_jph_diphone)";
	    } else if (streq(argv[1], "tll")) {
		voice_cmd = "(voice_tll_diphone)";
	    } else {
		argerr("unknown voice: %s (perhaps try --eval)\n", argv[1]);
	    }
	} else if (streq("--gender", argv[0])) {
	    if (argv[1][0] == 'm') {
		/* Festival standard voice: "American English male; at least as good as ked" */
		voice_cmd = "(voice_kal_diphone)";
	    } else if (argv[1][0] == 'f') {
		/* Female American English from OGI (requires custom Festival) */
		voice_cmd = "(voice_tll_diphone)";
	    } else {
		argerr("bad gender: %s", argv[1]);
	    }
	} else if (streq("--eval", argv[0])) {
	    eval_cmd = argv[1];
	} else {
	    argerr("unknown option: %s (try --help)\n", argv[1]);
	}
	argc -= 2;
	argv += 2;
    }
}

void
usage()
{
    fprintf(stderr, "usage: %s [--libdir LIBDIR] [--gender m[ale]|f[emale]] [--voice rab|ked|kal|...] [--eval FORM]\n", progname);
    exit(0);
}

void
argerr(char *fmt, char *s)
{
    fprintf(stderr, "%s: ", progname);
    fprintf(stderr, fmt, s);
    exit(-1);
}

void
programExit(int status)
{
    // Nothing to do to cleanup festival, so just exit
    exit(status);
}

extern "C" {
void
sendToSynth(const char *str) {
    festival_say_text(str);
}
}
