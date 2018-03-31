;;;;
;;;; File: Systems/bob/system.lisp
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Systems;core;system")

(trips:def-trips-system :bob
  (:dfc-component	:lxm               #!TRIPS"src;LexiconManager;")
  (:dfc-component	:parser            #!TRIPS"src;Parser;")
  (:dfc-component       :im                #!TRIPS"src;NewIM;")
  (:dfc-component       :channelkb         #!TRIPS"src;ChannelKB;")
  ;;(:dfc-component       :discourse-context #!TRIPS"src;DiscourseContext;")
  (:dfc-component       :deepsemlex	   #!TRIPS"src;DeepSemLex;code;lib;")
  (:dfc-component       :dagent            #!TRIPS"src;BasicDialogueAgent;")
  (:dfc-component       :dummy		   #!TRIPS"src;Dummy;")   ;; see end of file for loading dummy messages
  )

;; add WebParser to the system when we have its source directory
(when (probe-file #!TRIPS"src;WebParser")
  (nconc (assoc :bob trips::*trips-systems*)
	 (list '(:dfc-component :webparser #!TRIPS"src;WebParser;"))))

;; Now load the system
(trips:load-trips-system)
;; this isn't part of a lisp component so we load it separately
(load #!TRIPS"src;TextTagger;drum-dsl-resources.lisp")

;; domain preferences
(load "domain-sense-preferences")
(load "domain-words.lisp")

;; Here are the BOB specific files for the Basic Dialogue Agent

;;(load "cps-states.lisp")
;;(load "CPS-actions.lisp")

;;;; extractor rules
(load "preprocessRules.lisp")
(load "DRUMtermRules.lisp")
(load "DRUMtermRules_add.lisp")
(load "DRUMRules_ev.lisp")
(load "DRUMRules_ev_add.lisp")
(load "DRUMRules_mod.lisp")
(load "DRUMRules_CC.lisp")
(load "bobRules.lisp")
(load "DRUMRules_misc.lisp")
(load "emptyRules.lisp")  ; an empty rules file just so I can check the results of the LF substitution

(load "symbolmapping.lisp")

;;  loading the dummy message handling

;; the :dummy component is used to fake certain message interactions during 
;; system development.
;; if you need to use either of the following Dummy features, uncomment them 
;; LOCALLY, but please do not commit without comments!
;(load  #!TRIPS"src;Systems;bob;dummymessages")
;(load  #!TRIPS"src;Systems;bob;dummy-for-CSM")

(defun parse-eval (x)
  (im::send-msg `(request :receiver parser :content (eval ,x))))
