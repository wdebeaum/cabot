/*
 * pronunciation.h
 *
 * Lucian Galescu <lgalescu@ihmc.us>, 28 Apr 2005
 * $Id: pronunciation.h,v 1.2 2014/02/23 22:53:43 iperera Exp $
 */

#ifndef _pronunciation_h
#define _pronunciation_h

#include <ApplicationServices/ApplicationServices.h>

//Doesn't work on new mac's gcc
//#pragma options align=mac68k

#pragma pack(push, 2)

typedef struct dict_entry_field_s {
    short length;		/* not including pad byte in data, if needed */
    short type;			/* $0021=text of word, $0022=phonetic text */
    /* data follows */		/* padded to even length */
} dict_entry_field;

enum {
    DICT_ENTRY_FIELD_TYPE_TEXT = 0x0021,
    DICT_ENTRY_FIELD_TYPE_PHON = 0x0022,
};

typedef struct dict_entry_s {
    short length;		/* including this header */
    short type;			/* $0021=pronunciation, $0022=abbreviation */
    short number_of_fields;
    /* Fields follow */
} dict_entry;

enum {
    DICT_ENTRY_TYPE_PRON = 0x0021,
    DICT_ENTRY_TYPE_ABBR = 0x0022,
};

typedef struct dict_resource_s {
    int length;			/* including this header */
    int type;			/* 'dict' */
    int version;		/* 1 */
    short script_code;		/* eg., 'smRoman' */
    short language_code;	/* eg., langEnglish */
    short region_code;		/* eg., verUS */
    unsigned long modification_date;	/* seconds since 01/01/1904 00:00*/
    char reserved[16];		/* should be set to 0 */
    int entry_count;
    /* entries follow */
} dict_resource;

#pragma options align=reset

typedef struct pronunciation_s {
    char *text;
    char *phon;
} pronunciation;

extern int debuglevel;

/* for debugging: write binary data to file */
static void dump (FILE *fp, char *data, int nitems);

/* reads pronunciation dictionary from file
 * returns number of entries read, -1 on error */
static int readDictionary(char *dictfile, pronunciation **dict);

/* creates a pronunciation dictionary from a list of word-pronunciation pairs 
   returns handle to dictionary */
Handle createPronunciationDictionary(pronunciation *items, int nitems);

/* install pronunciation dictionary */
int installPronunciationDictionary(SpeechChannel channel, char *dictfile);
#endif
