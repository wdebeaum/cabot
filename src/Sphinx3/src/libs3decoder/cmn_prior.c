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
 * **********************************************
 * 
 * 30-Dec-2000  Rita Singh (rsingh@cs.cmu.edu) at Carnegie Mellon University
 * Created
 */


#include "cmn_prior.h"

#include "libutil/cmd_ln.h"

void cmn_prior(float32 **incep, int32 varnorm, int32 nfr, int32 ceplen, 
	       int32 endutt)
{
  static float32 *cur_mean = NULL; /* the mean subtracted from input frames */
  static float32 *sum = NULL;	     /* the sum over input frames */
  static int32   nframe;	     /* the total number of input frames */
  static int32   initialize=1;
  float32 sf;
  int32   i, j;
  char *cm_file; /* lgalescu: see below */
  int32 cmn_from_file = 0;

  if (varnorm)
    E_FATAL("Variance normalization not implemented in live mode decode\n");

  if (initialize){
    cur_mean = (float32 *) ckd_calloc(ceplen, sizeof(float32));

    /* 2004/08/21 L. Galescu <lgalescu@ihmc.us> 
     * For some reason, it takes a long segment of speech for new means to be 
     * obtained; the defaults here are waaay off!! This seems to be the reason 
     * that recognition is always poor for the first utterance, and it can be 
     * poor for many an utterance in live mode.
     * I found these means to be remarkably consistent (but this needs further 
     * checking). So I just decided to save them into a file and load them at 
     * initialization.
     */

    if ((cm_file = cmd_ln_str("-cmfn")) != NULL) {
      FILE *cmfp;

      cmn_from_file = 1;
      if ((cmfp = fopen(cm_file, "r")) == NULL) {
	E_WARN("Could not open file %s. Using default cmn priors.\n", cm_file);
	cmn_from_file = 0;
      } else {
	for (i = 0; i < ceplen; i++) {
	  if (fscanf(cmfp, "%f", cur_mean + i) == EOF) {
	    E_WARN("Error reading cmn_prior[%d]. Using default cmn priors.\n", i);
	    cmn_from_file = 0;
	  }
	}
	fclose(cmfp);
      }
    }

    if (! cmn_from_file) {
      /* A front-end dependent magic number */
      cur_mean[0] = 12.0;
      for (i = 1; i < ceplen; i++) {
	cur_mean[i] = 0;
      }
    }

    sum      = (float32 *) ckd_calloc(ceplen, sizeof(float32));
    nframe   = 0;
    initialize = 0;
    for (i = 0; i < ceplen; i++) 
      E_INFO("mean[%d]= %.2f\n", i, cur_mean[i]);
  }
	
  /* lgalescu 2004/08/22 -- well, if no new frames, but this is end of utt, 
   * don't we want to update the means buffer?
   */
  /*
  if (nfr <= 0)
    return;
  */
  if (nfr > 0) {

    for (i = 0; i < nfr; i++){
      for (j = 0; j < ceplen; j++){
	sum[j] += incep[i][j];
	incep[i][j] -= cur_mean[j];
      }
      ++nframe;
    }

    /* Shift buffer down if we have more than CMN_WIN_HWM frames */
    if (nframe > CMN_WIN_HWM) {
      sf = (float32) (1.0/nframe);
      for (i = 0; i < ceplen; i++)
	cur_mean[i] = sum[i] * sf;

      /* Make the accumulation decay exponentially */
      if (nframe >= CMN_WIN_HWM) {
	sf = CMN_WIN * sf;
	for (i = 0; i < ceplen; i++)
	  sum[i] *= sf;
	nframe = CMN_WIN;
      }
    }
  }

  if (endutt) {
    /* Update mean buffer */

    {char tmp[1024];
      sprintf(tmp, "cmn_prior_update: from < ");
      for (i = 0; i < ceplen; i++)
        sprintf(tmp+strlen(tmp), "%5.2f ", cur_mean[i]);
      sprintf(tmp+strlen(tmp), ">\n");
      E_INFO(tmp);
    }

    sf = (float32) (1.0/nframe);
    for (i = 0; i < ceplen; i++)
      cur_mean[i] = sum[i] * sf;

    /* Make the accumulation decay exponentially */
    if (nframe > CMN_WIN_HWM) {
      sf = CMN_WIN * sf;
      for (i = 0; i < ceplen; i++)
	sum[i] *= sf;
      nframe = CMN_WIN;
    }

    {char tmp[1024];
      sprintf(tmp, "cmn_prior_update: to   < ");
      for (i = 0; i < ceplen; i++)
        sprintf(tmp+strlen(tmp), "%5.2f ", cur_mean[i]);
      sprintf(tmp+strlen(tmp), ">\n");
      E_INFO(tmp);
    }

    /* lgalescu: save the cepstral means 
     * -- they may be useful for initialization (see above) 
     */
    if (!save_cm_to_file(cur_mean, ceplen))
      E_WARN("Current means not saved.\n");
  }

}

/* lgalescu 2004/08/22 */
int32 save_cm_to_file(float32 *cep_means, int32 ceplen)
{
  int32 i;
  FILE *cmfp;
  char *cmfn = cmd_ln_str("-cmsave");

  /* be sure we only call this function when it makes sense */
  if (cmfn == NULL) 
    return 0;

  if ((cmfp = fopen(cmfn, "w")) == NULL) {
    E_WARN("Could not open file %s. Cepstral means not saved.\n", cmfn);
    return 0;
  }

  for (i = 0; i < ceplen; i++) {
    fprintf(cmfp, "cur_mean[%d] = %.2f\n", i, cep_means[i]);
  }

  fclose(cmfp);

  return 1;
}
