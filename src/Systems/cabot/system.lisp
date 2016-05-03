;;;;
;;;; File: system.lisp
;;;;
;;;; Defines and loads an instance of the TRIPS system for CABOT 
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Systems;core;system")

(trips:def-trips-system :cabot
  (:old-trips-component :lxm               #!TRIPS"src;LexiconManager;")
  (:dfc-component	:parser            #!TRIPS"src;Parser;")
  (:dfc-component       :im                #!TRIPS"src;NewIM;")
  (:dfc-component       :dagent            #!TRIPS"src;BasicDialogueAgent;")
  (:dfc-component       :llearner          #!TRIPS"src;LanguageLearner;")
  ;; the :dummy component is used to fake certain message interactions during
  ;; system development. comment out when not needed!
  (:dfc-component       :dummy		   #!TRIPS"src;Dummy;")
  )

;; add WebParser to the system when we have its source directory
(when (probe-file #!TRIPS"src;WebParser")
  (nconc (assoc :cabot trips::*trips-systems*)
	 (list '(:dfc-component :webparser #!TRIPS"src;WebParser;"))))

;; Now load the system
(trips:load-trips-system)

;;PARSER, TEXTTAGGER and WORDFINDER settings

(parser::setmaxchartsize 5000)
(setf parser::*include-parse-tree-in-messages* '(w::lex)) ; for WebParser

(setq *use-texttagger* T)
(setq im::*current-dialog-manager* #'im::textIM)
(setq im::*output-format* 'im::LF)
(setq wf::*use-wordfinder* t)
(setf (parser::number-parses-to-find parser::*chart*) 2)
(setf (parser::number-parses-desired parser::*chart*) 1)
(setq parser::*filter-and-preparse-input* nil)
(parser::setmaxchartsize 3000)
(setf (parser::flexible-semantic-matching parser::*chart*) t)

;;  IM settings  -- we are using the basic dialogue IM 
(setq im::*current-dialog-manager* #'im::simpleIM)
(setq im::*output-format* 'im::LF)
(setq im::*external-name-resolution* nil)  ;; will eventually be set to T when we do reference resolution in context
(setq im::*max-allowed-utts-in-turn* 2) ;; we're being a little generous to try to pick up more referring expressions
(setq im::*cps-control* t)
(im::trace-on 1)

;;  DM settings
(setq dagent::*silent-failures* nil)  ;; don't ignore utterance failure
(setq dagent::*using-alarms* nil)   ;; no alarms
(setq dagent::*disabled-wizard* t)  ;; no wizard
(dagent::trace-on 1)

; just the default user
(setq dagent::*users* (list (cons "desktop" (dagent::make-user :name "desktop" :channel-id 'dagent::desktop))))

;; and here's the state definitions for the dialogue manager

;;(load "Sallerules.lisp")
(load "cps-states.lisp")

;;  loading the dummy message handling

(load "dummymessages.lisp")
