
#include <sys/stat.h>
#include <fcntl.h>
#include "live2.h"
#include "ad.h"

#define BUFSIZE 4096

live_decoder_t decoder;

static
int16 utterance_loop()
{
    ad_rec_t *in_ad = 0;
    int16 samples[BUFSIZE];
    int32 num_samples;
    char *hypstr;
    char buffer[1024];
    
    int uttno = 0;
    char uttid[64];
    char uttfn[1024];
    FILE *rawfp;
    
    int32 sample_rate = cmd_ln_int32 ("-samprate");
    int32 tot_samples;
    int32 max_samples = 1 * sample_rate;

    in_ad = ad_open_sps(sample_rate);

    for (;;)
    {

        tot_samples = 0;

        uttno++;
        sprintf(uttid, "utt%05d", uttno);
        sprintf(uttfn, "%s/%s.raw", cmd_ln_str("-outrawdir"), uttid);
        rawfp = fopen(uttfn, "wb");
        if (ld_utt_begin(&decoder, uttid)) {
            printf(">>>>> ld_utt_begin() failed.\n");
            return -1;
        }

        printf("press ENTER to start recording\n");
        fgets(buffer, 1024, stdin);

        ad_start_rec(in_ad);

        while (tot_samples < max_samples) {
            num_samples = ad_read(in_ad, samples, BUFSIZE);
            if (num_samples && rawfp)
                fwrite (samples, sizeof(int16), num_samples, rawfp);
            if (num_samples > 0) {
                if (ld_utt_proc_raw(&decoder, samples, num_samples) < 0) {
                    printf(">>>>> ld_utt_proc_raw() returned unexpectedly.\n");
                    return -1;
                }
                if (ld_utt_hyps(&decoder, &hypstr, 0)) {
                    printf(">>>>> ld_utt_hyps() failed\n");
                }
                else {
                    printf(">>>>> decoder returned:\n%s\n", hypstr);
                }
                tot_samples += num_samples;
            }
        }

        ad_stop_rec(in_ad);
        fclose(rawfp);

        if (ld_utt_end(&decoder)) {
            printf(">>>>> ld_utt_end() failed.\n");
            return -1;
        }

        if (ld_utt_hyps(&decoder, &hypstr, 0)) {
            printf(">>>>> ld_utt_hyps() failed\n");
        }
        else {
            printf(">>>>> decoder returned:\n%s\n", hypstr);
        }
    }
    
    ad_close(in_ad);
    
    return 0;
}

int
main(int argc, char **argv)
{
    /////////////////////////////////////////////////////////////////////////////
    // Initializing
    //
    if (argc < 2) {
        printf("Usage: s3livedecoder [ARGS] \n");
        return -1;
    }

    if (ld_init(&decoder, argc, argv)) {
        printf(">>>>> ld_init() failed.\n");
        return -1;
    }

    /////////////////////////////////////////////////////////////////////////////
    // Main loop
    //
    utterance_loop();
    
    /////////////////////////////////////////////////////////////////////////////
    // The end
    //
    ld_finish(&decoder);
    
    return 0;
}
