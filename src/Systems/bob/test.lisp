;;;;
;;;; File: Systems/bob/test.lisp
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

;;;
;;; Load our TRIPS system
;;;
(defvar *no-im* nil)
(load (if *no-im*
	#!TRIPS"src;Systems;bob;system-no-im"
	#!TRIPS"src;Systems;bob;system"))

;;;
;;; In USER for convenience of testing
;;;
(in-package :common-lisp-user)

;;;
;;; Enable TextTagger
;;;
(setf *use-texttagger* t)

;;;
;;; Sample dialogues
;;;
(defvar *sample-dialogues*
  '((ben-model-test .
     ("Let's build a model of the RAS neighborhood."
      "The receptor tyrosine kinase EGFR binds the growth factor ligand EGF."
      "The EGFR-EGF complex binds another EGFR-EGF complex."
      "The EGFR-EGFR complex binds GRB2."
      "GRB2 bound to EGFR binds SOS1 that is not phosphorylated."
      "GRB2-bound SOS1 that is not phosphorylated binds NRAS that is not bound to BRAF."
      "SOS1-bound NRAS binds GTP."
      "GTP-bound NRAS that is not bound to SOS1 binds BRAF."
      "BRAF binds BRAF."
      "Vemurafenib binds BRAF."
      "Phosphorylated MAP2K1 is activated."
      "PP2A-alpha dephosphorylates MAP2K1 that is not bound to ERK2."
      "Active MAP2K1 that is not bound to PP2A-alpha phosphorylates ERK2."
      "Phosphorylated ERK2 is activated."
      "DUSP6 dephosphorylates ERK2 that is not bound to SOS1."
      "Phosphatases dephosphorylate SOS1."
      "BRAF V600E that is not bound to Vemurafenib phosphorylates MAP2K1."
      ))
    (Nov-demo .
     ("I have a patient with pancreatic cancer. What drug should I use?"
      ;; SYS: 88% of cancer patient have a mutation in KRAS that makes it constitutively activated
      ;; SYS: But I don't know any drugs targeting KRAS
      "Let's look at the KRAS neighborhood"
      ;; SYS: OK
      "I know that KRAS activates Raf, which activates Erk."
      ;; SYS: OK <displays mechanism>
      "Erk activation drives cancer progression"
      ;; SYS: OK <updates mechanism>
      "Are there any known Raf inhibitors?"
      ;; SYS: Dabrafenib, vemurafenib, GDC-0879 and dabrafenib mesylate are known RAF inhibitors 
      "Is Erk inactivated if I add vemurafenib?"
      ;; SYS: Yes, my model shows that Erk is inactivated. 
      ;; SYS: But the model is incomplete. There is negative feedback from Erk to KRAS. Thwn the feedback loop is included in the model, ERK remains active.
      ))
    (Mar-demo .
     ("I want to find a molecular treatment for pancreatic cancer."
      "What drug could I use?"
      ;; SYS: I can't find a drug to treat pancreatic cancer.
      "What proteins might lead to the development of pancreatic cancer?"
      ;; SYS: 88% of cancer patient have a mutation in KRAS that makes it active
      "Are there any drugs targeting KRAS?"
      ;; SYS: I don't know any drugs targeting KRAS
      "Let's build a model of the KRAS neighborhood."
      ;; SYS: OK
      "I know that KRAS activates Raf, Raf activates Mek and Mek activates Erk."
      ;; SYS: OK <displays mechanism>
      (TELL :content (DISPLAY-AVAILABLE :what "filename"))
      "Erk activation drives cancer progression"
      ;; SYS: OK <updates mechanism display>
      "Is Erk inactivated if I add Selumetinib?"
      ;; SYS: <display mechanism with Selumetinib>
      ;; SYS: Yes, Erk is inactivated by Selumetinib, which is a MEK inhibitor.
      ;; 8<----------------------------------- END OF DEMO (rest is future work)
      ;; SYS: <<checks literature for connection b/w Selumetinib and cancer progression>>
      ;; SYS: But PMC3942828 suggests that Selumetinib induces ErbB signaling, which leads to cancer progression.
      ))
    (Mar-demo-fake .
     (;; USR: "I want to find a molecular treatment for pancreatic cancer."
      ;; NB: SNOMEDCT not yet integrated;
      ;;     BA could map EUI:E0223598 to SNOMEDCT:372142002 if DTDA needs it
      ;; not sending DEFINE-TYPE yet
      ;(TELL :CONTENT
      ;    (DEFINE-TYPE :content ONT::PANCREATIC-CANCER 
      ;     :subtype ONT::ILLNESS :dbid "SNOMEDCT:372142002"))
      (REQUEST :CONTENT 
	 (EVALUATE :CONTENT (ADOPT :WHAT ONT::V32410 :AS (GOAL))
		   :CONTEXT ((ONT::RELN ONT::V32410 :INSTANCE-OF ONT::DETERMINE :NEUTRAL ONT::V32473 :DRUM -)
			     (ONT::RELN ONT::V32473 :INSTANCE-OF ONT::TREATMENT :DISEASE ONT::V32525 :DRUM - :MOD ONT::V32470)
			     (ONT::RELN ONT::V32470 :INSTANCE-OF ONT::MODIFIER :OF ONT::V32473 :WNSENSE "molecular%3:01:00::" :FORCE ONT::TRUE)
			     (ONT::A ONT::V32525 :INSTANCE-OF ONT::PANCREATIC-CANCER :DRUM ((:DRUM (SPECIALIST :EUI E0223598 :CITATION-FORM "pancreatic cancer" :MATCHES ((MATCH :SCORE 1.0 :MATCHED "pancreatic cancer" :EXACT 2))))))))
	 :REPLY-WITH IO-32953)
      ;; from BA: ACCEPTABLE
      ;;(TELL :RECEIVER DAGENT :CONTENT
      ;;      (REPORT :CONTENT (ACCEPTABLE :WHAT (ADOPT :WHAT ONT::V32410 :AS (GOAL))))
      ;;      :IN-REPLY-TO IO-32953 :sender BA)
      (REQUEST :CONTENT 
	       (COMMIT :CONTENT (ADOPT :WHAT ONT::V32410 :AS (GOAL))
		       :CONTEXT ((ONT::RELN ONT::V32410 :INSTANCE-OF ONT::DETERMINE :NEUTRAL ONT::V32473 :DRUM -)
				 (ONT::RELN ONT::V32473 :INSTANCE-OF ONT::TREATMENT :DISEASE ONT::V32525 :DRUM - :MOD ONT::V32470)
				 (ONT::RELN ONT::V32470 :INSTANCE-OF ONT::MODIFIER :OF ONT::V32473 :WNSENSE "molecular%3:01:00::" :FORCE ONT::TRUE)
				 (ONT::A ONT::V32525 :INSTANCE-OF ONT::PANCREATIC-CANCER :DRUM ((:DRUM (SPECIALIST :EUI E0223598 :CITATION-FORM "pancreatic cancer" :MATCHES ((MATCH :SCORE 1.0 :MATCHED "pancreatic cancer" :EXACT 2)))))))))
      (REQUEST :CONTENT
	       (GENERATE :CONTENT (ONT::ACCEPT)))
      ;; SYS: "OK"
      ;; USR: "What drug could I use?"
      (REQUEST :CONTENT
	       (EVALUATE :CONTENT (ADOPT :WHAT ONT::I33272 :AS (SUBGOAL :OF ONT::V32410))
			 :CONTEXT ((ONT::RELN ONT::I33272 :INSTANCE-OF ONT::IDENTIFY :AFFECTED ONT::V32966)
				   (ONT::A ONT::V32966 :INSTANCE-OF ONT::MEDICATION :NAME W::DRUG :DRUM - :SUCHTHAT ONT::V33006)
				   (ONT::RELN ONT::V33006 :INSTANCE-OF ONT::USE :AFFECTED ONT::V32966 :DRUM - :TENSE ONT::PRES)))
	       :REPLY-WITH IO-33273)
      ;; from BA: ACCEPTABLE
      ;;(TELL :RECEIVER DAGENT :CONTENT
      ;;      (REPORT :CONTENT (ACCEPTABLE :WHAT (ADOPT :WHAT I33272 :AS (SUBGOAL :OF ONT::V32410))))
      ;;      :IN-REPLY-TO IO-33273 :sender BA)
      (REQUEST :CONTENT
	       (COMMIT :CONTENT (ADOPT :WHAT ONT::I33272 :AS (SUBGOAL :OF ONT::V32410))
		       :CONTEXT ((ONT::RELN ONT::I33272 :INSTANCE-OF ONT::IDENTIFY :AFFECTED ONT::V32966)
				 (ONT::A ONT::V32966 :INSTANCE-OF ONT::MEDICATION :NAME W::DRUG :DRUM - :SUCHTHAT ONT::V33006)
				 (ONT::RELN ONT::V33006 :INSTANCE-OF ONT::USE :AFFECTED ONT::V32966 :DRUM - :TENSE ONT::PRES))))

      (REQUEST :CONTENT
	       (WHAT-NEXT :ACTIVE-GOAL ONT::I33272)
	       :REPLY-WITH IO-33274)
      ;; from BA: FAILURE
      ;;(TELL :RECEIVER DAGENT :CONTENT
      ;;      (REPORT :CONTENT (FAILURE :WHAT F1 :AS (SUBGOAL :OF I33272))
      ;;	      :CONTEXT ((A F1 :INSTANCE-OF ONT::LOOK-UP :NEUTRAL ONT::V32966)
      ;;		        (ONT::RELN I33272 :INSTANCE-OF ONT::IDENTIFY :AFFECTED ONT::V32966)
      ;;		        (ONT::A ONT::V32966 :INSTANCE-OF ONT::MEDICATION :NAME W::DRUG :DRUM - :SUCHTHAT ONT::V33006)
      ;;		        (ONT::RELN ONT::V33006 :INSTANCE-OF ONT::USE :AFFECTED ONT::V32966 :DRUM - :TENSE ONT::PRES)))
      ;;      :IN-REPLY-TO IO-33274 :sender BA)
      (REQUEST :CONTENT
	       (GENERATE :CONTENT (ONT::TELL :CONTENT RF1)
			 :CONTEXT ((RELN RF1 :INSTANCE-OF ONT::FAIL :PURPOSE F1)
				   (A F1 :INSTANCE-OF ONT::LOOK-UP :FORMAL I33272))))
      ;; SYS: "I can't find a drug to treat pancreatic cancer."
      ;; USR: "What proteins might lead to the development of pancreatic cancer?"
      (REQUEST :CONTENT
	       (EVALUATE :CONTENT (ADOPT :WHAT ONT::I34939 :AS (SUBGOAL :OF ONT::I33272))
			 :CONTEXT ((ONT::RELN ONT::I34939 :INSTANCE-OF ONT::IDENTIFY :AFFECTED ONT::V33303)
				   (ONT::A ONT::V33303 :INSTANCE-OF ONT::PROTEIN :NAME W::PROTEIN :DRUM - :SUCHTHAT ONT::V33320)
				   (ONT::RELN ONT::V33320 :INSTANCE-OF ONT::CAUSE-EFFECT :AGENT ONT::V33303 :RESULT ONT::V33361 :TENSE ONT::PRES :MODALITY (:* ONT::POSSIBILITY ONT::MIGHT) :WNSENSE "lead%2:42:04::" :FORCE ONT::POSSIBLE)
				   (ONT::RELN ONT::V33361 :INSTANCE-OF ONT::INCREASE :AFFECTED ONT::V33398 :DRUM -)
				   (ONT::A ONT::V33398 :INSTANCE-OF ONT::PANCREATIC-CANCER :DRUM ((:DRUM (SPECIALIST :EUI E0223598 :CITATION-FORM "pancreatic cancer" :MATCHES ((MATCH :SCORE 1.0 :MATCHED "pancreatic cancer" :EXACT 2))))))))
	       :REPLY-WITH IO-34940)
      ;; from BA: ACCEPTABLE
      ;;(TELL :RECEIVER DAGENT :CONTENT
      ;;      (REPORT :CONTENT (ACCEPTABLE :WHAT (ADOPT :WHAT I34939 :AS (SUBGOAL :OF I33272))))
      ;;      :IN-REPLY-TO IO-34940 :sender BA)
      (REQUEST :CONTENT
	       (COMMIT :CONTENT (ADOPT :WHAT ONT::I34939 :AS (SUBGOAL :OF ONT::I33272))
		       :CONTEXT ((ONT::RELN ONT::I34939 :INSTANCE-OF ONT::IDENTIFY :AFFECTED ONT::V33303)
				 (ONT::A ONT::V33303 :INSTANCE-OF ONT::PROTEIN :NAME W::PROTEIN :DRUM - :SUCHTHAT ONT::V33320)
				 (ONT::RELN ONT::V33320 :INSTANCE-OF ONT::CAUSE-EFFECT :AGENT ONT::V33303 :RESULT ONT::V33361 :TENSE ONT::PRES :MODALITY (:* ONT::POSSIBILITY ONT::MIGHT) :WNSENSE "lead%2:42:04::" :FORCE ONT::POSSIBLE)
				 (ONT::RELN ONT::V33361 :INSTANCE-OF ONT::INCREASE :AFFECTED ONT::V33398 :DRUM -)
				 (ONT::A ONT::V33398 :INSTANCE-OF ONT::PANCREATIC-CANCER :DRUM ((:DRUM (SPECIALIST :EUI E0223598 :CITATION-FORM "pancreatic cancer" :MATCHES ((MATCH :SCORE 1.0 :MATCHED "pancreatic cancer" :EXACT 2)))))))))
      (REQUEST :CONTENT
	       (GENERATE :CONTENT (ONT::ACCEPT)))
      ;; SYS: ??? "I can find that for you" ???
      (REQUEST :CONTENT
	       (WHAT-NEXT :ACTIVE-GOAL ONT::I34939)
	       :REPLY-WITH IO-34941)
      ;; from BA: SOLUTION
      ;;(TELL :RECEIVER DAGENT :CONTENT
      ;;      (REPORT :CONTENT (SOLUTION :WHAT E1 :GOAL I34939 :JUSTIFICATION R01)
      ;; 	      :CONTEXT ((RELN E1 :INSTANCE-OF ONT::EQUALS :NEUTRAL ONT::V33303 :NEUTRAL1 PK01)
      ;; 			(A PK01 :INSTANCE-OF ONT::PROTEIN :NAME KRAS :DBID P01116)
      ;; 			(RELN R01 :INSTANCE-OF ONT::HAVE :NEUTRAL Q1 :NEUTRAL1 MUT1)
      ;; 			(QUANT Q1 :INSTANCE-OF ONT::PATIENT :QUAN N2 :DOMAIN S2)
      ;; 			(A N2 :INSTANCE-OF ONT::PERCENTAGE :VALUE 88)
      ;; 			(A S2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE P2)
      ;; 			(KIND P2 :INSTANCE-OF ONT::PATIENT :MODS (R2))
      ;; 			(RELN R2 :INSTANCE-OF ONT::HAVE :NEUTRAL P2 :NEUTRAL1 C01)
      ;; 			(A C01 :INSTANCE-OF ONT::PANCREATIC-CANCER)
      ;; 			(A MUT1 :INSTANCE-OF ONT::MUTATION :MODS (RM1 RM2))
      ;; 			(RELN RM1 :INSTANCE-OF ONT::HAVE :NEUTRAL PK01 :NEUTRAL1 MUT1)
      ;; 			(RELN RM2 :INSTANCE-OF ONT::CAUSE-EFFECT :AGENT RM1 :RESULT RA1)
      ;; 			(RELN RA1 :INSTANCE-OF ONT::ACTIVATE :AGENT MUT1 :AFFECTED PK01)))
      ;;      :IN-REPLY-TO IO-34941 :sender BA)
      (REQUEST :CONTENT
	       (GENERATE :CONTENT (ONT::TELL :CONTENT E1)
			 :CONTEXT ((RELN E1 :INSTANCE-OF ONT::EQUALS :NEUTRAL ONT::V33303 :NEUTRAL1 PK01)
				   (A PK01 :INSTANCE-OF ONT::PROTEIN :NAME KRAS :DBID P01116)
				   (RELN R01 :INSTANCE-OF ONT::HAVE :NEUTRAL Q1 :NEUTRAL1 MUT1)
				   (QUANT Q1 :INSTANCE-OF ONT::PATIENT :QUAN N2 :DOMAIN S2)
				   (A N2 :INSTANCE-OF ONT::PERCENTAGE :VALUE 88)
				   (A S2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE P2)
				   (KIND P2 :INSTANCE-OF ONT::PATIENT :MODS (R2))
				   (RELN R2 :INSTANCE-OF ONT::HAVE :NEUTRAL P2 :NEUTRAL1 C01)
				   (A C01 :INSTANCE-OF ONT::PANCREATIC-CANCER)
				   (A MUT1 :INSTANCE-OF ONT::MUTATION :MODS (RM1 RM2))
				   (RELN RM1 :INSTANCE-OF ONT::HAVE :NEUTRAL PK01 :NEUTRAL1 MUT1)
				   (RELN RM2 :INSTANCE-OF ONT::CAUSE-EFFECT :AGENT RM1 :RESULT RA1)
				   (RELN RA1 :INSTANCE-OF ONT::ACTIVATE :AGENT MUT1 :AFFECTED PK01))))
      ;; SYS: 88% of cancer patients have a mutation in KRAS that makes it active
      ;; USR: "Are there any drugs targeting KRAS?"
      (REQUEST :CONTENT
	       (EVALUATE :CONTENT (ADOPT :WHAT ONT::I35366 :AS (SUBGOAL :OF ONT::I34939))
			 :CONTEXT ((ONT::RELN ONT::I35366 :INSTANCE-OF ONT::IDENTIFY :AFFECTED ONT::V34972)
				   (ONT::A ONT::V34972 :INSTANCE-OF ONT::MEDICATION :NAME W::DRUG :DRUM - :MOD ONT::V34976)
				   (ONT::RELN ONT::V34976 :INSTANCE-OF ONT::MODULATE :NEUTRAL ONT::V34972 :NEUTRAL1 ONT::V34982 :DRUM -)
				   (ONT::A ONT::V34982 :INSTANCE-OF ONT::GENE :NAME W::KRAS :DRUM ((:DRUM (TERM :ID HGNC::|6407| :NAME "Kirsten rat sarcoma viral oncogene homolog" :SCORE 1.0 :MATCHES ((MATCH :SCORE 1.0 :MATCHED "KRAS" :STATUS "Approved Symbol" :EXACT 1)) :DBXREFS (UP::P01116)))))))
	       :REPLY-WITH IO-35367)
      ;; from BA: ACCEPTABLE
      ;;(TELL :RECEIVER DAGENT :CONTENT
      ;;      (REPORT :CONTENT (ACCEPTABLE :WHAT (ADOPT :WHAT I35366 :AS (SUBGOAL :OF I34939))))
      ;;      :IN-REPLY-TO IO-35367 :sender BA)
      (REQUEST :CONTENT
	       (COMMIT :CONTENT (ADOPT :WHAT ONT::I35366 :AS (SUBGOAL :OF ONT::I34939))
		       :CONTEXT ((ONT::RELN ONT::I35366 :INSTANCE-OF ONT::IDENTIFY :AFFECTED ONT::V34972)
				 (ONT::A ONT::V34972 :INSTANCE-OF ONT::MEDICATION :NAME W::DRUG :DRUM - :MOD ONT::V34976)
				 (ONT::RELN ONT::V34976 :INSTANCE-OF ONT::MODULATE :NEUTRAL ONT::V34972 :NEUTRAL1 ONT::V34982 :DRUM -)
				 (ONT::A ONT::V34982 :INSTANCE-OF ONT::GENE :NAME W::KRAS :DRUM ((:DRUM (TERM :ID HGNC::|6407| :NAME "Kirsten rat sarcoma viral oncogene homolog" :SCORE 1.0 :MATCHES ((MATCH :SCORE 1.0 :MATCHED "KRAS" :STATUS "Approved Symbol" :EXACT 1)) :DBXREFS (UP::P01116))))))))
      (REQUEST :CONTENT
	       (GENERATE :CONTENT (ONT::ACCEPT)))
      ;; SYS: ???  "I will try to find that for you" ???
      (REQUEST :CONTENT
	       (WHAT-NEXT :ACTIVE-GOAL ONT::I35366)
	       :REPLY-WITH IO-35368)
      ;; from BA: FAILURE
      ;;(TELL :RECEIVER DAGENT :CONTENT
      ;;      (REPORT :CONTENT (FAILURE :WHAT F1 :AS (SUBGOAL :OF I35366))
      ;; 	      :CONTEXT ((A F1 :INSTANCE-OF ONT::LOOK-UP :NEUTRAL ONT::V34972)
      ;; 			(ONT::RELN I35366 :INSTANCE-OF ONT::IDENTIFY :AFFECTED ONT::V34972)
      ;; 			(ONT::A ONT::V34972 :INSTANCE-OF ONT::MEDICATION :NAME W::DRUG :DRUM - :MOD ONT::V34976)
      ;; 			(ONT::RELN ONT::V34976 :INSTANCE-OF ONT::MODULATE :NEUTRAL ONT::V34972 :NEUTRAL1 ONT::V34982 :DRUM -)
      ;; 			(ONT::A ONT::V34982 :INSTANCE-OF ONT::GENE :NAME W::KRAS :DRUM ((:DRUM (TERM :ID HGNC::|6407| :NAME "Kirsten rat sarcoma viral oncogene homolog" :SCORE 1.0 :MATCHES ((MATCH :SCORE 1.0 :MATCHED "KRAS" :STATUS "Approved Symbol" :EXACT 1)) :DBXREFS (UP::P01116)))))))
      ;;      :IN-REPLY-TO IO-35368 :sender BA)
      (REQUEST :CONTENT
	       (GENERATE :CONTENT (ONT::TELL :CONTENT RF1)
			 :CONTEXT ((RELN RF1 :INSTANCE-OF ONT::FAIL :PURPOSE F1)
				   (A F1 :INSTANCE-OF ONT::LOOK-UP :FORMAL I35366))))
      ;; SYS: "I don't know any drugs targeting KRAS"
      ;; USR: "Let's build a model of the KRAS neighborhood."
      (REQUEST :CONTENT
	       (EVALUATE :CONTENT (ADOPT :WHAT ONT::V35502 :AS (SUBGOAL :OF ONT::I35366))
			 :CONTEXT ((ONT::RELN ONT::V35502 :INSTANCE-OF ONT::CREATE :AFFECTED-RESULT ONT::V35553 :DRUM -)
				   (ONT::A ONT::V35553 :INSTANCE-OF ONT::MODEL :OF ONT::V35576 :EXTENT ONT::SMALL :DRUM - NIL ONT::V35583)
				   (ONT::A ONT::V35583 :INSTANCE-OF ONT::DISTRICT :NAME W::NEIGHBORHOOD :DRUM - :ASSOC-WITH ONT::V35576)
				   (ONT::A ONT::V35576 :INSTANCE-OF ONT::GENE :NAME W::KRAS :DRUM ((:DRUM (TERM :ID HGNC::|6407| :NAME "Kirsten rat sarcoma viral oncogene homolog" :SCORE 1.0 :MATCHES ((MATCH :SCORE 1.0 :MATCHED "KRAS" :STATUS "Approved Symbol" :EXACT 1)) :DBXREFS (UP::P01116)))) :EQUALS ONT::V34982)))
	       :REPLY-WITH IO-36520)
      ;; from BA: ACCEPTABLE
      ;;(TELL :RECEIVER DAGENT :CONTENT
      ;;      (REPORT :CONTENT (ACCEPTABLE :WHAT (ADOPT :WHAT ONT::V35502 :AS (SUBGOAL :OF I35366))))
      ;;      :IN-REPLY-TO IO-36520 :sender BA)
      (REQUEST :CONTENT
	       (COMMIT :CONTENT (ADOPT :WHAT ONT::V35502 :AS (SUBGOAL :OF ONT::I35366))
		       :CONTEXT ((ONT::RELN ONT::V35502 :INSTANCE-OF ONT::CREATE :AFFECTED-RESULT ONT::V35553 :DRUM -)
				 (ONT::A ONT::V35553 :INSTANCE-OF ONT::MODEL :OF ONT::V35576 :EXTENT ONT::SMALL :DRUM -)
				 (ONT::A ONT::V35576 :INSTANCE-OF ONT::GENE :NAME W::KRAS :DRUM ((:DRUM (TERM :ID HGNC::|6407| :NAME "Kirsten rat sarcoma viral oncogene homolog" :SCORE 1.0 :MATCHES ((MATCH :SCORE 1.0 :MATCHED "KRAS" :STATUS "Approved Symbol" :EXACT 1)) :DBXREFS (UP::P01116)))) :EQUALS ONT::V34982))))
      (REQUEST :CONTENT
	       (GENERATE :CONTENT (ONT::ACCEPT)))
      ;; SYS: "OK"
      ;; USR: "I know that KRAS activates Raf, which activates Mek, and Mek activates Erk."
      ;; Note: the two mentions of MEK get different terms! the two mentions of RAF generate a single term.
      (REQUEST :CONTENT
	       (EVALUATE :CONTENT (ASSERTION :WHAT ONT::I39642 :AS (CONTRIBUTES-TO :GOAL ONT::V35514))
			 :CONTEXT ((ONT::RELN ONT::I39642 :INSTANCE-OF ONT::EVENTS-IN-MODEL :EVENTS (ONT::V36637 ONT::V36583 ONT::V36608))
				   (ONT::RELN ONT::V37988 :INSTANCE-OF ONT::S-CONJOINED :OPERATOR ONT::AND :SEQUENCE (ONT::V36544 ONT::V36637) :FORCE ONT::TRUE)
				   (ONT::RELN ONT::V36544 :INSTANCE-OF ONT::KNOW :EXPERIENCER ONT::V36530 :FORMAL ONT::V36583 :TENSE ONT::PRES :WNSENSE "know%2:31:01::" :FORCE ONT::TRUE)
				   (ONT::RELN ONT::V36637 :INSTANCE-OF ONT::ACTIVATE :AGENT ONT::V36633 :AFFECTED ONT::V36644 :DRUM - :TENSE ONT::PRES)
				   (ONT::A ONT::V36530 :INSTANCE-OF ONT::PERSON :PRO ONT::I :EQUALS ONT::USER)
				   (ONT::RELN ONT::V36583 :INSTANCE-OF ONT::ACTIVATE :AGENT ONT::V36575 :AFFECTED ONT::V36590 :DRUM - :TENSE ONT::PRES)
				   (ONT::A ONT::V36575 :INSTANCE-OF ONT::GENE :NAME W::KRAS :DRUM ((:DRUM (TERM :ID HGNC::|6407| :NAME "Kirsten rat sarcoma viral oncogene homolog" :SCORE 1.0 :MATCHES ((MATCH :SCORE 1.0 :MATCHED "KRAS" :STATUS "Approved Symbol" :EXACT 1)) :DBXREFS (UP::P01116)))) :EQUALS ONT::V34994)
				   (ONT::A ONT::V36590 :INSTANCE-OF ONT::PROTEIN-FAMILY :NAME W::RAF :DRUM ((:DRUM (TERM :ID FA::|03114| :NAME "RAF subfamily" :SCORE 0.99587 :MATCHES ((MATCH :SCORE 0.99587 :MATCHED "RAF" :STATUS "ID" :INITIAL-CAP-ALL-CAPS 1))) (SPECIALIST :EUI E0319446 :CITATION-FORM "RAF" :MATCHES ((MATCH :SCORE 0.85529 :MATCHED "RAF" :INITIAL-CAP-ALL-CAPS 1)) :ABBREVIATION-OF ((SPECIALIST :EUI E0319447 :CITATION-FORM "repetitive atrial firing") (SPECIALIST :EUI E0319448 :CITATION-FORM "rheumatoid arthritis factor")) :ACRONYM-OF ((SPECIALIST :EUI E0707877 :CITATION-FORM "relative activity factor") (SPECIALIST :EUI E0707878 :CITATION-FORM "Royal Air Force"))) (SPECIALIST :EUI E0707902 :CITATION-FORM "Raf" :MATCHES ((MATCH :SCORE 1.0 :MATCHED "Raf" :EXACT 1)) :ABBREVIATION-OF ((SPECIALIST :EUI E0051880 :CITATION-FORM "raffinose"))) (SPECIALIST :EUI E0707903 :CITATION-FORM "Raf" :MATCHES ((MATCH :SCORE 1.0 :MATCHED "Raf" :EXACT 1))))) :MOD ONT::V36608)
				   (ONT::RELN ONT::V36608 :INSTANCE-OF ONT::ACTIVATE :AGENT ONT::V36590 :AFFECTED ONT::V36615 :DRUM - :TENSE ONT::PRES)
				   (ONT::A ONT::V36615 :INSTANCE-OF ONT::MOLECULE :NAME W::MEK :DRUM ((:DRUM (TERM :ID CHEBI::|28398| :NAME "butan-2-one" :SCORE 0.65301 :MATCHES ((MATCH :SCORE 0.65301 :MATCHED "MEK" :STATUS "RELATED synonym" :INITIAL-CAP-ALL-CAPS 1)) :MAPPINGS ((MAP :THROUGH (CONCEPT CHEBI::|23367| (SENSE (MORPH (POS N) (WORD (MOLECULAR ENTITI))) (PROVENANCE (NAME LEXICON-DATA::CHEBI) (FILENAME "/Users/lgalescu/work/Projects/active/2015 CwC/bob/src/config/lisp/../../../src/TextTagger/drum-dsl/CHEBI/002336.lisp")))) :TO ONT::MOLECULE))) (SPECIALIST :EUI E0566199 :CITATION-FORM "MEK" :MATCHES ((MATCH :SCORE 0.85529 :MATCHED "MEK" :INITIAL-CAP-ALL-CAPS 1)) :ACRONYM-OF ((SPECIALIST :EUI E0566198 :CITATION-FORM "MAP-ERK kinase") (SPECIALIST :EUI E0650476 :CITATION-FORM "methylethylketone"))))))
				   (ONT::A ONT::V36633 :INSTANCE-OF ONT::MOLECULE :NAME W::MEK :DRUM ((:DRUM (TERM :ID CHEBI::|28398| :NAME "butan-2-one" :SCORE 0.65301 :MATCHES ((MATCH :SCORE 0.65301 :MATCHED "MEK" :STATUS "RELATED synonym" :INITIAL-CAP-ALL-CAPS 1)) :MAPPINGS ((MAP :THROUGH (CONCEPT CHEBI::|23367| (SENSE (MORPH (POS N) (WORD (MOLECULAR ENTITI))) (PROVENANCE (NAME LEXICON-DATA::CHEBI) (FILENAME "/Users/lgalescu/work/Projects/active/2015 CwC/bob/src/config/lisp/../../../src/TextTagger/drum-dsl/CHEBI/002336.lisp")))) :TO ONT::MOLECULE))) (SPECIALIST :EUI E0566199 :CITATION-FORM "MEK" :MATCHES ((MATCH :SCORE 0.85529 :MATCHED "MEK" :INITIAL-CAP-ALL-CAPS 1)) :ACRONYM-OF ((SPECIALIST :EUI E0566198 :CITATION-FORM "MAP-ERK kinase") (SPECIALIST :EUI E0650476 :CITATION-FORM "methylethylketone"))))))
				   (ONT::A ONT::V36644 :INSTANCE-OF ONT::GENE :NAME W::ERK :DRUM ((:DRUM (TERM :ID HGNC::|6871| :NAME "mitogen-activated protein kinase 1" :SCORE 0.65301 :MATCHES ((MATCH :SCORE 0.65301 :MATCHED "ERK" :STATUS "Synonym" :INITIAL-CAP-ALL-CAPS 1)) :DBXREFS (UP::P28482)) (TERM :ID HGNC::|3393| :NAME "EPH receptor B2" :SCORE 0.31015 :MATCHES ((MATCH :SCORE 0.31015 :MATCHED "ERK" :STATUS "Previous Symbol" :INITIAL-CAP-ALL-CAPS 1)) :DBXREFS (UP::P29323)) (SPECIALIST :EUI E0321287 :CITATION-FORM "ERK" :MATCHES ((MATCH :SCORE 0.85529 :MATCHED "ERK" :INITIAL-CAP-ALL-CAPS 1)) :ACRONYM-OF ((SPECIALIST :EUI E0321288 :CITATION-FORM "extracellular signal regulated kinase"))))))))
	       :REPLY-WITH IO-39645)
      
      ;; *** DAGENT handles everything up to here ***
      ;; *** DUMMY replies with (TELL :RECEIVER DAGENT :CONTENT NIL :IN-REPLY-TO IO-39645) ***
      
      ;; SYS: OK <displays mechanism>
      (TELL :content (DISPLAY-AVAILABLE :what "filename"))
      ;; USR: "Erk activation drives cancer progression"
      ;; SYS: OK <updates mechanism display>
      ;; USR: "Is Erk inactivated if I add Selumetinib?"
      ;; SYS: <display mechanism with Selumetinib>
      ;; SYS: Yes, Erk is inactivated by Selumetinib, which is a MEK inhibitor.
      ;; 8<----------------------------------- END OF DEMO (rest is future work)
      ;; SYS: <<checks literature for connection b/w Selumetinib and cancer progression>>
      ;; SYS: But PMC3942828 suggests that Selumetinib induces ErbB signaling, which leads to cancer progression.
      ))
    ))

;;; Load core testing code (this needs to be the last load so the "type (run)"
;;; message appears right before the prompt)
;;;
(load #!TRIPS"src;Systems;core;test")

;; Default sample dialogue for this domain
(setf *test-dialog*
   (cdr (assoc 'Mar-demo *sample-dialogues*)))

(defun ptest (key)
  "Make the sample dialogue given by KEY the global *TEST-DIALOG*, then
call TEST. Reports available KEYs on error."
  (let ((dialogue (cdr (assoc key *sample-dialogues* :test #'eql))))
    (cond
     ((not dialogue)
      (format t "~&ptest: unknown sample dialogue: ~S~%" key)
      (format t "~&ptest: possible values: ~S~%" (mapcar #'car *sample-dialogues*)))
     (t
      (setf *test-dialog* dialogue)
      (test)))))

(defun run-im-only ()
  (dfc:run-component :im))

