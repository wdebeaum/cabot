;;;;
;;;; defsys.lisp for Dagent- the generic dialogue agent 
;;;;
;;;;;;;; Time-stamp: <Wed Aug  1 14:27:10 EDT 2012 jallen>
;;;;


;; Note: this dialogue manager is driven by files that are
;;  loaded in the system folder that define valid user channels and 
;;  the dialogue manager actions. 

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :parser)
  (load #!TRIPS"src;Parser;defsys"))

(unless (find-package :IM)
  (load #!TRIPS"src;NewIM;defsys"))

(mk:defsystem :llearner-code
    :source-pathname #!TRIPS"src;LanguageLearner;"
    :components ( "messages"
		  "warn"
		  "learner"
		  ))

(dfc:defcomponent :llearner
  :use (:common-lisp :util)
  :system (:depends-on (:util :parser :im :llearner-code)))


(dfc:defcomponent-method dfc:init-component :after ()
  ;;(initialize)
  )


(defun run ()
  (dfc:run-component :llearner)
  )
