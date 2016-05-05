;;;;
;;;; defsys.lisp for Dagent- the generic dialogue agent 
;;;;
;;;;;;;; Time-stamp: <Mon Dec 28 16:13:07 EST 2015 jallen>
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

(mk:defsystem :dagent-code
    :source-pathname #!TRIPS"src;BasicDialogueAgent;"
    :components ( "messages"
		  "warn"
		  "StateManager"
		  "CPSstate"
		  "user-db"
		  ))

;;(mk:defsystem :dagent-data
   ;; :source-pathname #!TRIPS"src;BasicDialogueAgent;"
;;   :components ("states"))

(dfc:defcomponent :dagent
  :use (:common-lisp :util)
  :system (:depends-on (:util :dagent-code :parser :im)))


(dfc:defcomponent-method dfc:init-component :after ()
  (initialize)
 ;; (start-dagent)
  )


;; Dummy run -- the real component is the domain specific D-agent
(defun run ()
  (dfc:run-component :dagent)
  )


(defvar *me*)

(defvar *user*)
;;