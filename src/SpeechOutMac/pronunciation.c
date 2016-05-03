/*
 * pronunciation.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 15 Dec 2004
 * Time-stamp: <Mon Dec 21 14:42:23 EST 2009 ferguson>
 *
 * Compile with:

    gcc -g -Wpadded -o pron pronunciation.c -framework ApplicationServices

  * Need to watch alignment of the dict_resource structure where an int
  * follows an odd number of 2-byte shorts...

 * For more info:

   /System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/SpeechSynthesis.framework/Versions/A/Headers/SpeechSynthesis.h

   file:///Developer/ADC%20Reference%20Library/documentation/Carbon/Reference/Speech_Synthesis_Manager/index.html#//apple_ref/doc/uid/TP30000211

   /Developer/Examples/Speech/Synthesis/

 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ApplicationServices/ApplicationServices.h>

#include "pronunciation.h"

static void
syserr(char *str, int err)
{
    fprintf(stderr, "%s: error %d\n", str, err);
    exit(err);
}

static void
dump (FILE *fp, char *data, int nitems)
{
    unsigned char *p; 
    int i;
    for (p = data, i=0; i < nitems; p++,i++) {
	fprintf(fp, "%2.2X", *p );
	if (i % 2 == 1) fprintf(fp, " ");
	if (i % 16 == 15) fprintf(fp, "\n");
    }
    fprintf(fp, "\n");
}

static int
pad(int n)
{
    if (n % 2 == 1) {
	return n + 1;
    } else {
	return n;
    }
}

static int
field_size(char *s)
{
    return sizeof(dict_entry_field) + pad(strlen(s));
}

static int
set_field(dict_entry_field *field, short type, char *s)
{
    char *data;

    /* length not including pad byte */
    field->length = sizeof(dict_entry_field) + strlen(s);
    field->type = type;
    data = (char*)field + sizeof(dict_entry_field);
    strcpy(data, s);

    if (debuglevel > 8) {
	fprintf(stderr, "New field [%d]:\n", field->length);
	dump(stderr, (char *) field, field->length);
    }

    return pad(field->length);
}

static int
entry_size(char *text, char *phon)
{
    return sizeof(dict_entry) + field_size(text) + field_size(phon);
}

static int
set_entry(dict_entry *entry, char *text, char *phon)
{
    char *data;

    entry->length = entry_size(text, phon);
    entry->type = DICT_ENTRY_TYPE_PRON;
    entry->number_of_fields = 2;
    data = (char*)entry + sizeof(struct dict_entry_s);
    data += set_field((dict_entry_field *)data, DICT_ENTRY_FIELD_TYPE_TEXT, text);
    data += set_field((dict_entry_field *)data, DICT_ENTRY_FIELD_TYPE_PHON, phon);

    if (debuglevel > 8) {
	fprintf(stderr, "New entry [%d]:\n", entry->length);
	dump(stderr, (char *) entry, entry->length);
    }

    return entry->length;
}

/*
 * read entry in pronunciation dictionary
 * returns: number of tokens, 2 means success 
 * side effects: word in wptr[0] and pron in wptr[1]
 * input line format: \s*(WORD)\s+(PRON)\n?
 * the trick here is that PRON may have spaces in it (just not at the beginning)
 * WARNING: this function modifies its first argument!
 */
static int
readDicEntry(char *line, char **wptr)
{
    char *w;
    int n, i;

    /* chop \n; there should at most one, at the end */
    if ((w = strchr(line, '\n')) != NULL)
	*w = '\0';

    n = 0; 
    /* get first token */
    for (w = (char *) line; (wptr[n] = strsep(&w, " \t\n")) != NULL ;) 
	if (*wptr[n] != '\0') {
	    n++;
	    break;
	}
    if (w != NULL) {
	/* skip any blanks */
	for(; (*w != '\0') && isblank(*w); w++);
	/* get the rest */
	if (*w != '\0')
	    wptr[n++] = w;
    }

    /* check */
    if (debuglevel > 7) {
	for(i = 0; i < n; i++) 
	    printf("[%d] :%s:\n", i, wptr[i]);
    }

    return n;
}

/*
 * reads pronunciation dictionary from file
 * returns number of entries read, -1 on error
 */
static int
readDictionary(char *dictfile, pronunciation **dict)
{
    FILE *fp;
    int n, lineno;
    char line[256], **wptr, *p;
    int maxwn = 2;

    if (! dictfile) {
	fprintf(stderr, "No dictionary!\n");
	return -1;
    }
    if ((fp = fopen(dictfile, "r")) == NULL) {
	fprintf(stderr, "Failed to open dictionary %s.\n", dictfile);
	return -1;
    }

    n = 0;
    while (fgets (line, sizeof(line), fp) != NULL) {
	if (line[0] != '#')
	    n++;
    }
    rewind(fp);

    if (n == 0) {
	fclose(fp);
	return n;
    }
  
    *dict = (pronunciation *) calloc(n, sizeof(pronunciation));

    wptr = (char **) calloc(maxwn, sizeof(char *));

    n = lineno = 0;
    while (fgets (line, sizeof(line), fp) != NULL) {
	lineno++;
	if (line[0] == '#')
	    continue;
	if (readDicEntry(line, wptr) != 2) {
	    /* very gentle... humans make mistakes... */
	    fprintf(stderr, "Erroneous line skipped: [%d] %s\n", lineno, line);
	    continue;
	}

	(*dict)[n].text = strdup(wptr[0]);
	(*dict)[n].phon = strdup(wptr[1]);
	n++;
    }
    fclose(fp);

    free(wptr);

    if (debuglevel > 0) {
	fprintf(stderr, "%d entries read\n", n);
	if (debuglevel > 1) {
	    int i;
	    for(i=0; i<n; i++) 
		fprintf(stderr, "[%d]\t%s\t%s\n", 
			i, (*dict)[i].text, (*dict)[i].phon);
	}
    }

    return n;
}

Handle
createPronunciationDictionary(pronunciation *items, int nitems)
{
    int dict_size;
    dict_resource *dict;
    char *data;
    int i;
    Handle handle;

    if (debuglevel > 1) 
      fprintf(stderr, "creating dict with %d entries\n", nitems); 

    /* Compute total size needed */
    dict_size = sizeof(dict_resource);
    for (i=0; i < nitems; i++) {
	dict_size += entry_size(items[i].text, items[i].phon);
    }
    if (debuglevel > 1) 
      fprintf(stderr, "dict_size = %d\n", dict_size); 

    /* Allocate memory (using MacOS Memory manager routine) */
    handle = NewHandle(dict_size);
    if (handle == NULL) {
	perror("NewHandle");
	return NULL;
    }

    dict = (dict_resource*)(*handle);

    /*
    fprintf(stderr, "allocated dict @ 0x%lx\n", dict);
    fprintf(stderr, " length @ 0x%lx\n", &(dict->length));
    fprintf(stderr, " type @ 0x%lx\n", &(dict->type));
    fprintf(stderr, " version @ 0x%lx\n", &(dict->version));
    fprintf(stderr, " script_code @ 0x%lx\n", &(dict->script_code));
    fprintf(stderr, " language_code @ 0x%lx\n", &(dict->language_code));
    fprintf(stderr, " region_code @ 0x%lx\n", &(dict->region_code));
    fprintf(stderr, " modification_date @ 0x%lx\n", &(dict->modification_date));
    fprintf(stderr, " reserved @ 0x%lx\n", &(dict->reserved));
    fprintf(stderr, " entry_count @ 0x%lx\n", &(dict->entry_count));
    */

    /* dict_resource fields */
    dict->length = dict_size;
    strncpy((char*)&(dict->type), "dict", 4);
    dict->version = 1;
    dict->script_code = smRoman;		/* == 0, from Script.h */
    dict->language_code = langEnglish;		/*  "          "       */
    dict->region_code = verUS;			/*  "          "       */
    //GetDateTime(&dict->modification_date);
    memset((char*)(&dict->reserved), '\0', 16);
    dict->entry_count = nitems;

    if (debuglevel > 2) {
	fprintf(stderr, " length = %u\n", dict->length);
	fprintf(stderr, " type = %X (\"%.4s\")\n", dict->type, (char*)&(dict->type));
	fprintf(stderr, " version = %u\n", dict->version);
	fprintf(stderr, " script_code = %X\n", dict->script_code);
	fprintf(stderr, " language_code = %X\n", dict->language_code);
	fprintf(stderr, " region_code = %X\n", dict->region_code);
	fprintf(stderr, " modification_date = %lX\n", dict->modification_date);
	fprintf(stderr, " entry_count = %u\n", dict->entry_count);
    }

    /* Set entries */
    data = (char*)dict + sizeof(dict_resource);
    for (i=0; i < nitems; i++) {
	data += set_entry((dict_entry *)data, items[i].text, items[i].phon);
    }

    /* Done */
    if (debuglevel > 5) 
	fprintf(stderr, "returning Handle 0x%lx\n", handle);

    if (debuglevel > 6) {
	FILE *fd;
	fd = fopen("dict.dump", "w");
	dump(fd, (char *) dict, dict->length);
	fclose(fd);
    }

    return handle;
}

/* one-shot installation of pronunciation dictionary 
   returns number of entries added, or -1 on failure */
int 
installPronunciationDictionary(SpeechChannel channel, char *dictfile)
{
    int err;
    Handle dict;
    int n_entries;
    pronunciation *dic;

    if ((n_entries = readDictionary(dictfile, &dic)) < 0) {
	fprintf(stderr, "Warning: No dictionary will be installed.\n");
	return -1;
    }

    if (n_entries > 0) {
	dict = createPronunciationDictionary(dic, n_entries);

	if ((err = UseDictionary(channel, dict)) != noErr) {
	    syserr("UseDictionary", err);
	}

	free(dic);
	DisposeHandle(dict);
    }

    return n_entries;
}

/*
 * for testing
 */
#ifdef TESTING

int debuglevel = 0;

static char *items[] = {
    "celebrex", "OK", "shakespeare", "sheik"
};
static int nitems = 4;

static void
speak_items(SpeechChannel channel)
{
    int i, err;

    for (i=0; i < nitems; i++) {
	char *text = items[i];
	printf("%s\n", text);
	if ((err = SpeakText(channel, text, strlen(text))) != noErr) {
	    syserr("SpeakText", err);
	}
	sleep(2);
    }
}

int
main(int argc, char **argv)
{
    SpeechChannel channel;
    int err;
    char *progname;
    char *dictfile = NULL;

    if ((progname = strrchr(argv[0], '/')) == NULL) {
	progname = argv[0];
    } else {
	progname += 1;
    }
    /*
     * parse args
     */
    while (--argc > 0) {
	argv += 1;
	if (strcmp(argv[0], "-debuglevel") == 0) {
	    if (argc < 2) {
		fprintf(stderr, "missing value for -debuglevel");
	    } else {
		if (sscanf(argv[1], "%d", &debuglevel) != 1)
		    debuglevel = 0;
		argc -= 1;
		argv += 1;
	    }
	} else if (strcmp(argv[0], "-dic") == 0) {
	    if (argc < 2) {
		fprintf(stderr, "missing value for -dic");
	    } else {
		dictfile = argv[1];
		argc -= 1;
		argv += 1;
	    }	    
	} else {
	    fprintf(stderr, "usage: %s [-debuglevel n] [-dic dictfile]\n", 
		    progname);
	    exit(1);
	}
    }

    /*
     * Create speech channel (NULL means default voice)
     */
    if ((err = NewSpeechChannel(NULL, &channel)) != noErr) {
	syserr("NewSpeechChannel", err);
    }
    /*
     * Speak once without dictionary
     */
    speak_items(channel);

    /*
     * install pronunciation dictionary 
     */
    if (installPronunciationDictionary(channel, dictfile) > 0) {
    
	/*
	 * Speak again with dictionary
	 */
	speak_items(channel);
    }

    /*
     * Cleanup
     */
    DisposeSpeechChannel(channel);
    exit(0);
}

#endif
