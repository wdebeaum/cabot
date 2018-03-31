;;;
;;; After-load customizations for this TRIPS system
;;;

;; BOB uses text-tagger
(setq *use-texttagger* t)

;;;; Parser options
(setq parser::*parser-init-settings*
      '((parser::*parser-display-enabled* nil)
	(parser::*in-system* :bob)
	;; average number of letters in a word (not critical)
	(parser::*word-length* 8)
	;; boost factor based on length of input covered
	(parser::*score-length-multiplier* .4)
	;; not clear this is helpful
	(parser::*score-corner-multiplier* 0)
	;; indicate we should use POS information
	(parser::*use-tags-as-filter* t)
	;; penalty multiplier for lex entries that do not match POS tags
	(parser::*bad-tag-multiplier* .9)
	;; penalty for referential-sem lex items
	(parser::*referential-sem-penalty* .98)
	;; constituents that we expect in the skeleton
	(parser::*skeleton-constit-cats* '(W::NP W::CP W::VP W::ADVBL W::PP W::S))
	;; boost constituents that match the skeleton (from stat. parser)
	(parser::*skeleton-boost-factor* 1.04)
	;; penalty for crossing skeleton constituent boundaries
	((setf (parser::barrier-penalty parser::*chart*) .99))
	;;
	(parser::*use-senses-as-filter* t)
	;;
	(parser::*bad-sense-multiplier* .96)
	;;
	(parser::*no-positions-in-lf* nil)
	;;

;	(parser::*beam-pruning-on* nil)   ; no pruning
	(parser::*beam-pruning-on* t)

	;;
;	(parser::*pruning-frequency* 500)
	(parser::*pruning-frequency* 1000)
	;;
	(parser::*beam-width* 20)
	;; max number of constituents built before stopping
;	((parser::setmaxnumberentries 2000))
	((parser::setmaxnumberentries 6000))
	;;
	((parser::setmaxchartsize 8000))
	;;
	(parser::*kr-type-info-desired* '(:WNsense :DRUM :wordnet))
	;;
	((setf (parser::flexible-semantic-matching parser::*chart*) t))
	;; boost content words that have domain specific info attached
	(parser::*domain-boosting-factor* 1.01)
	;; have the parser remove tagged constituents that are subparts of other terms with domain info
	(parser::*filter-texttagger-input* t)
	;; number of interpretations to obtain before stopping
	((setf (parser::number-parses-to-find parser::*chart*) 4))
	;; number of interpretations returned
	((setf (parser::number-parses-desired parser::*chart*) 1))
	;; disfavor speech acts not common in text
	(parser::*filter-and-preparse-input* t)   ;; enable preparsing (e.g., for sequences)
	((parser::customize-cost-table 
	  '((ont::SA_QUERY 1.2) 
	     (ont::SA_IDENTIFY 1.3) 
	     (ont::SA_pred-fragment 2) 
	     (ont::SA_request 1) 
	     (ont::SA_YN-QUESTION 1) 
	     (w::ADJP 1.2) 
	     (w::advbl 1.3)
	     (ont::SA_CONFIRM 1) 
	     (ont::SA_WH-QUESTION 1) 
	     (ont::SA_TELL 1)
	     (w::CP 2) 
	     (w::VP 1.1) 
	     (w::punc .5))))
       ))

(parser::initialize-settings)   ;; do it here to set the parser for testing (s o it matches the settings of the parser component)

;;;; IM options
;; dialogue manager, eg: textIM, simpleIM, extractIM...
(setq im::*current-dialog-manager* #'im::SequentialLFTransformIM)   ;;#'im::simpleIM)
(setq im::*CPS-control* t) ;; the CPS agent will control the sending of additional speech acts from an utterance

;(setq im::*substitute-types-in-pros* t)
(setq im::*substitute-types-in-pros* nil)
(setq im::*compute-force-from-tmas* t)

;; how fragmented do we allow the input to be before giving up
(setq im::*max-allowed-utts-in-turn* 3)
;; no domain-specific reasoner
(setq im::*external-name-resolution* nil)
;; pass on the LF structures utterance by utterance (rather than by paragraph)
(setq im::*lf-output-by-utterance* t)
;; generate LF graphs? (turn on for debugging)
(setq im::*show-lf-graphs* t)
;; we want to extract multiple events even when they share arguments
(setq im::*max-cover-with-strict-disjoint-extractions* nil)
;; we want to emit all events
(setq im::*eliminate-subevents* nil)

(setq im::*allow-optional-lfs* t) ;; set to t for optional term matching
(setq im::*output-format* 'im::lf-term)

;(setq im::*symbol-map* nil)

;; this looks obsolete
;(setq ex::*extraction-ids* '(ex::drum-extraction))

(setq *print-pretty* t)

(setq *texttagger-split-mode*
  nil
;  :split-clauses
;  :split-sentences
  )

;;;; LxM options
;; use WordFinder?
(setq lxm::*use-wordfinder* t)
;; we are trying to really depend on the Stanford parser (for now!)
(setq lxm::*use-tagged-senses-only* t)
;; don't use wordnet if we have domain-specific info from TextTagger
(setq lxm::*no-wf-senses-for-words-tagged-with-ont-types* t)
;; don't use wordnet if we have TRIPS entries  
(setq  lxm::*use-trips-and-wf-senses* nil) 
;; lower bound
(setq lxm::*domain-score-lower-bound* 0.95)

;;;; LOGGING options
(setq logging::*logging-enabled* nil)
(setq logging2::*logging-enabled* nil)


;; Here's the CPS agent settings


(im::trace-on 1)

; bobRules is before drum so that the domain specific EVENT rules take precedence (and also because bobRules also contains TERM extractions) (and also because CREATE is matched for both the BOB-specific CREATE event and for the general drum ACTIVATE)
; now add bobrules to drum instead since we need to preserve the AGENT pronoun + non-molecular AFFECTED-RESULT
(setq im::*extraction-sequence* '((im::preprocessRules) (im::drumterms) (im::drumtermsAdd) ;(im::bobRules)
				  (im::drum) (im::drum_ev_add) (im::drummod) (im::drumCC) (im::drumMisc) (im::emptyRules)))
(setq im::*substitute-terms-in-extraction* t)
(setq im::*roles-to-emit* nil)

(dagent::trace-on 1)
(setq dagent::*silent-failures* nil)
(setq dagent::*disabled-wizard* t)     ;; no wizard
(setq dagent::*using-alarms* t) 

; just the default user
(setq dagent::*users* (list (cons "desktop" (dagent::make-user :name "desktop" :channel-id 'dagent::desktop))))

