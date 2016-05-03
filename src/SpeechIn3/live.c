/* ====================================================================
 * Copyright (c) 1999-2001 Carnegie Mellon University.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * This work was supported in part by funding from the Defense Advanced 
 * Research Projects Agency and the National Science Foundation of the 
 * United States of America, and the CMU Sphinx Speech Consortium.
 *
 * THIS SOFTWARE IS PROVIDED BY CARNEGIE MELLON UNIVERSITY ``AS IS'' AND 
 * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY
 * NOR ITS EMPLOYEES BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ====================================================================
 *
 */
/*************************************************
 * CMU ARPA Speech Project
 *
 * Copyright (c) 2000 Carnegie Mellon University.
 * ALL RIGHTS RESERVED.
 *************************************************
 *
 * 13-Oct-2004	Lucian Galescu <lgalescu@ihmc.us> at IHMC
 *		Fix: in live_get_partialhyp(), only if the vithist id is 0 
 *		should the partial hyp be cleared, otherwise keep the 
 *		current one.
 *		
 * 05-Sep-2004	Lucian Galescu <lgalescu@ihmc.us> at IHMC
 *		Added option whether to report noise (-reportfill).
 *		
 * 01-Sep-2004	Lucian Galescu <lgalescu@ihmc.us> at IHMC
 *		Added option whether to report pronunciations (-reportpron).
 *		
 * 27-Aug-2004  Lucian Galescu <lgalescu@ihmc.us> at IHMC
 *		Added saving raw audio file in live_utt_decode_block().
 *
 * 24-Aug-2004  Lucian Galescu <lgalescu@ihmc.us> at IHMC
 *		Externalized parthyplen (length of partial hyp); this obviously 
 *	        belongs with parthyp.
 *
 * 13-Apr-2001  Ricky Houghton
 *              Added live_free_memory to clean up memory allocated locally.
 *
 * 01-Jan-2000  Rita Singh (rsingh@cs.cmu.edu) at Carnegie Mellon University
 *		Created a separate function live_get_partialhyp() to 
 *		generate partial hypotheses from the kb structure
 *
 * 31-Dec-2000  Rita Singh (rsingh@cs.cmu.edu) at Carnegie Mellon University
 * Created
 */

#include <libutil/libutil.h>
#include "kb.h"
#include "utt.h"
#include "cmd_ln_args.h"
#include "new_fe.h"		   /* 01.15.01 - RAH, use new_fe.h instead */
#include "live.h"

static fe_t  *fe;

static kb_t  *kb;
static kbcore_t *kbcore;
static FILE  *hmmdumpfp;
static int32 maxwpf;
static int32 maxhistpf;
static int32 maxhmmpf;
static int32 ptranskip;

/* 
 * Data defined here 
 */
partialhyp_t *parthyp = NULL;
int32 parthyplen = 0;
int16 peak_amplitude;

static float32 *dummyframe;

/* This routine initializes decoder variables for live mode decoding */
void live_initialize_decoder(char *live_args)
{
    static kb_t live_kb;
    int32   maxcepvecs, maxhyplen, samprate, ceplen;
    param_t *fe_param;
    /*char const *uttIdNotDefined = "null";*/
    
    parse_args_file(live_args);
    unlimit();
    kb_init(&live_kb);
    kb = &live_kb;
    kbcore = kb->kbcore;
    
    kb->uttid = ckd_calloc(1000,sizeof(char));
    hmmdumpfp = cmd_ln_int32("-hmmdump") ? stderr : NULL;
    maxwpf    = cmd_ln_int32 ("-maxwpf");
    maxhistpf = cmd_ln_int32 ("-maxhistpf");
    maxhmmpf  = cmd_ln_int32 ("-maxhmmpf");
    ptranskip = cmd_ln_int32 ("-ptranskip");
    
    maxhyplen = cmd_ln_int32 ("-maxhyplen");
    if (!parthyp) 
        parthyp  = (partialhyp_t *) ckd_calloc(maxhyplen, sizeof(partialhyp_t));
    parthyplen = 0;
    
    fe_param = (param_t *) ckd_calloc(1, sizeof(param_t));
    samprate = cmd_ln_int32 ("-samprate");
    if (samprate != 8000 && samprate != 16000)
        E_FATAL("Sampling rate %d not supported. Must be 8000 or 16000\n",samprate);
    
    fe_param->SAMPLING_RATE = (float32) samprate;
    fe_param->LOWER_FILT_FREQ = cmd_ln_float32("-lowerf");
    fe_param->UPPER_FILT_FREQ = cmd_ln_float32("-upperf");
    fe_param->NUM_FILTERS = cmd_ln_int32("-nfilt");
    
    /* 20040413, by ARCHAN. Clear the hardwiring.
       Hmm. Many people say no to do this, I just can't take it. 
    */
    fe_param->FRAME_RATE = cmd_ln_int32("-frate");
    
    /*    fe_param->FRAME_RATE = 100; */
    fe_param->PRE_EMPHASIS_ALPHA = cmd_ln_float32("-alpha");
    fe_param->FFT_SIZE = cmd_ln_int32("-nfft");
    fe_param->WINDOW_LENGTH = cmd_ln_float32("-wlen");
    fe_param->doublebw=OFF;
    
    fe = fe_init(fe_param);
    if (!fe)
        E_FATAL("Front end initialization fe_init() failed\n");
    
    maxcepvecs = cmd_ln_int32 ("-maxcepvecs");
    ceplen = kbcore->fcb->cepsize;
    
    dummyframe = (float32*) ckd_calloc(1 * ceplen,sizeof(float32));	/*  */
}


/* RAH Apr.13.2001: Memory was being held, Added Call fe_close to release 
 * memory held by fe and then release locally allocated memory 
 */
int32 live_free_memory ()
{
    parse_args_free();		/* Free memory allocated during the argument parseing stage */
    fe_close (fe);			/*  */
    ckd_free(kb->uttid);	/* Free memory allocated in live_initialize_decoder() */
    kb_free (kb);			/*  */
    ckd_free ((void *) dummyframe);	/*  */
    ckd_free ((void *) parthyp);	/*  */
    return (0);
}


/*******************************************************************
 * This routine retrieves the part hypothesis from the kb structure
 * at any stage in the decoding. The "endutt" flag is needed to know
 * whether the utterance is to be considered terminated or not
 * The function stores the partial hypothesis in the global array
 * "parthyp" and returns the number of words in the hypothesis
 *******************************************************************/

int32 live_get_partialhyp(int32 endutt)
{
    int32 id, nwds;
    glist_t   hyp;
    gnode_t   *gn;
    hyp_t     *h;
    dict_t    *dict;
    
    dict = kbcore_dict (kb->kbcore);
    if (endutt)
        id = vithist_utt_end(kb->vithist, kb->kbcore);
    else
        id = vithist_partialutt_end(kb->vithist, kb->kbcore);
    
    if (id > 0) {
        hyp = vithist_backtrace(kb->vithist,id);
        
        for (gn = hyp,nwds=0; gn; gn = gnode_next(gn),nwds++) {
            h = (hyp_t *) gnode_ptr (gn);
            if (parthyp[nwds].word != NULL) {
                ckd_free(parthyp[nwds].word);
                parthyp[nwds].word = NULL;
            }
	    /* 20040905 L Galescu <lgalescu@ihmc.us>
	     * Report noise? If not, replace with silence word. */
	    if ((cmd_ln_int32("-reportfill") == 0) && 
		dict_filler_word(dict, h->id))
	      parthyp[nwds].word = strdup(dict_wordstr(dict, dict->silwid));
	    else
	      parthyp[nwds].word = strdup(dict_wordstr(dict, h->id));
	    /* 20040901 L Galescu <lgalescu@ihmc.us>
	     * Choice to report alternative pronunciations or not. */
	    if (cmd_ln_int32("-reportpron") == 0) 
	      dict_word2basestr(parthyp[nwds].word);
            parthyp[nwds].sf = h->sf;
            parthyp[nwds].ef = h->ef;
            parthyp[nwds].ascr = h->ascr;
            parthyp[nwds].lscr = h->lscr;
        }
        if (parthyp[nwds].word != NULL){
            ckd_free(parthyp[nwds].word);
            parthyp[nwds].word = NULL;
        }
	parthyplen = nwds;
	/* Free hyplist */
        for (gn = hyp; gn && (gnode_next(gn)); gn = gnode_next(gn)) {
            h = (hyp_t *) gnode_ptr (gn);
            ckd_free ((void *) h);
        }
        glist_free (hyp);
    } else if (id == 0) {
        nwds = 0;
        if (parthyp[nwds].word != NULL) {
            ckd_free(parthyp[nwds].word);
            parthyp[nwds].word = NULL;
        }
	parthyplen = nwds;
    } else {
      /* lgalescu 2004/10/13: nothing changed; we return the same hyp that was there before */
    }
    
    return(parthyplen);
}

void live_utt_set_uttid(char uttname[])
{
    strcpy(kb->uttid,uttname);
}


/* Routine to decode a block of incoming samples. A partial hypothesis
 * for the utterance upto the current block of samples is returned.
 * The calling routine has to inform the routine if the block of samples
 * being passed is the final block of samples for an utterance by
 * setting live_endutt to 1. On receipt of a live_endutt flag the routine
 * automatically assumes that the next block of samples is the beginning
 * of a new utterance 
 */

int32 live_utt_decode_block (int16 *samples, int32 nsamples, 
			     int32 live_endutt, partialhyp_t **ohyp)
{
    static int32 live_begin_new_utt = 1;
    static int32 frmno;
    static float32 ***live_feat = NULL;
    
    int32   live_nfr, live_nfeatvec;
    int32   nwds =0;
    float32 **mfcbuf;
    /*    int i,j;*/
    /* 2004/08/27 L Galescu <lgalescu@ihmc.us> -- added raw audio file saving */
    static char uttfn[1024];
    static FILE *rawfp = NULL;
    int16 block_peak_amplitude;

    if(live_feat==NULL)
        live_feat = feat_array_alloc (kbcore_fcb(kbcore), LIVEBUFBLOCKSIZE);
    
    if (live_begin_new_utt){
        fe_start_utt(fe);
        utt_begin (kb);
        frmno = 0;
        kb->nfr = 0;
        kb->utt_hmm_eval = 0;
        kb->utt_sen_eval = 0;
        kb->utt_gau_eval = 0;
        live_begin_new_utt = 0;
        sprintf(uttfn, "%s/%s.raw", cmd_ln_str("-outrawdir"), kb->uttid);
        rawfp = fopen(uttfn, "wb");
    }
    /* 10.jan.01 RAH, fe_process_utt now requires ***mfcbuf and it allocates the memory internally) */
    mfcbuf = NULL;

    /* LG 20080613 */
    block_peak_amplitude = get_peak_amplitude(samples, nsamples);
    if (block_peak_amplitude > peak_amplitude)
      peak_amplitude = block_peak_amplitude;
    E_INFO("segment peak %d\n",peak_amplitude);

    live_nfr = fe_process_utt(fe, samples, nsamples, &mfcbuf); /**/
    if (rawfp != NULL) {
      fwrite(samples, sizeof(int16), nsamples, rawfp);
      if (live_endutt)
	  fclose(rawfp);
    }

    if (live_endutt) {
        /* RAH, It seems that we shouldn't throw out this data */
        fe_end_utt(fe,dummyframe); /* Flush out the fe */
    }
#if 0
    E_INFO("Number frame after fe_process_utt %d\n",live_nfr);
    for(i=0;i<live_nfr;i++){
      printf("%d ",i);
      for(j=0;j<13;j++){
        printf("%f ",mfcbuf[i][j]);
        fflush(stdout);
      }
      printf("\n");
      fflush(stdout);
    }
#endif
    /* lgalescu 2004/08/22 -- i am under the impression that 
     * feat_s2mfc2feat_block() needs to be called at the end of utt 
     * even if no frames need processing
     */
    /* lgalescu 2004/10/13 -- rescinded the above */
    if(live_nfr>0){
      /* Compute feature vectors */
      live_nfeatvec = feat_s2mfc2feat_block(kbcore_fcb(kbcore), mfcbuf,
					    live_nfr, live_begin_new_utt,
					    live_endutt, live_feat);

#if 0   
    E_INFO ("live_nfeatvec: %ld\n",live_nfeatvec);
#endif
#if 0
      E_INFO("Current frame number %d, Number of frames %d, Number frame after feat_s2mfcfeat_block %d\n",frmno,live_nfr,live_nfeatvec);
      
      for(i=0;i<live_nfeatvec;i++){
        printf("%d\n",i);
        printf("Cep: ");
        fflush(stdout);
        for(j=0;j<13;j++){
	  printf("%f ",live_feat[i][0][j]);
	  fflush(stdout);
        }
        printf("\n");
        fflush(stdout);
        printf("Del: ");
        fflush(stdout);
        for(j=13;j<26;j++){
	  printf("%f ",live_feat[i][0][j]);
	  fflush(stdout); 
        }
        printf("\n");
        fflush(stdout);
        printf("Acc: ");
        fflush(stdout);
        for(j=26;j<39;j++){
	  printf("%f ",live_feat[i][0][j]);
	  fflush(stdout);
        }
        printf("\n");
        fflush(stdout);
        
      }
#endif
    
      /* decode the block */
      utt_decode_block (live_feat, live_nfeatvec, &frmno, kb, 
			maxwpf, maxhistpf, maxhmmpf, ptranskip, hmmdumpfp);

      /* lgalescu 2004/08/21
       * moved the following block out of the previous if(){} because we need 
       * the output even when no feature computation has to be done.
       */
      /* lgalescu 2004/10/13 -- rescinded */

      /* Pull out partial hypothesis */
      nwds =  live_get_partialhyp(live_endutt);
      *ohyp = parthyp;
      parthyplen = nwds;
    }

    /* Clean up */
    if (live_endutt) {
      live_begin_new_utt = 1;
      kb->tot_fr += kb->nfr;
      utt_end(kb);
    }
    else {
      live_begin_new_utt = 0;
    }
    
    /* I'm starting to think that fe_process_utt should not be allocating its 
     * memory, that or it should allocate some max and just keep on going, 
     * this idea of constantly allocating freeing memory seems dangerous to me.
     */
    /* 20040318 ARCHAN : It sounds extremely dangerous to me and I will 
     * eliminate it sometime. 
     */
    /* lgalescu: i second that! the memory issue needs to be investigated: after a run on linux, i noticed some 1.6M of memory having "disappeared"! */

    if(live_nfr>0){
      ckd_free_2d((void **) mfcbuf); /* RAH, this must be freed since fe_process_utt allocates it */
    }

    return(parthyplen);
}

void live_utt_summary(){
    /*tot: %.2f xCPU, %.2f xClk\n",*/
    E_INFO("SUMMARY:  %d fr;  %d sen, %d gau/fr, %.2f xCPU [%.2f xOvrhd];  %d hmm/fr, %d wd/fr, %.2f xCPU;  \n",
           kb->tot_fr,
           (int32)(kb->tot_sen_eval / kb->tot_fr),
           (int32)(kb->tot_gau_eval / kb->tot_fr),
           kb->tm_sen.t_tot_cpu * 100.0 / kb->tot_fr,
           kb->tm_ovrhd.t_tot_cpu * 100.0 / kb->tot_fr,
           (int32)(kb->tot_hmm_eval / kb->tot_fr),
           (int32)(kb->tot_wd_exit / kb->tot_fr),
           kb->tm_srch.t_tot_cpu * 100.0 / kb->tot_fr);
    
    /*	   tm.t_tot_cpu * 100.0 / kb->tot_fr,
    tm.t_tot_elapsed * 100.0 / kb->tot_fr);*/
}

/* LG 20050614 */
/* This routine updates the decoder's dict & LM */
void live_update_decoder(char *dictfile, char *lmfile)
{
    kb_update_lm(kb, dictfile, lmfile);
}

/* LG 20080613 */
/* Get the peak amplitude for a set of samples; return 0 for no samples */
int16 get_peak_amplitude(int16* samples, int32 nsamples) {
  int32 i;
  int16 sample;
  int16 peak = 0;
  for (i=0; i < nsamples; i++) {
    sample = samples[i];
    /* Take absolute value */
    /* lg 2008/05/22: bug fix: -(-32768) = -32768 !! */
    if (sample == MAX_NEG_INT16) sample = MAX_INT16;
    else if (sample < 0) sample = -sample;
    /* update peak */
    if (sample > peak) {
      peak = sample;
    }
  }
  E_INFO("block peak %d\n",peak);

  return peak;
}
