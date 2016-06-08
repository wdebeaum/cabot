;;  Managing the collaborative problem solving state
;;  This is sill hacked together to support the demos -- but the interfaces are correct

(in-package :dagent)

(defun set-system-goal (&key content context)
  (let ((user (lookup-user 'desktop)))
    ;; eventually get rid of the next two and do within the model
    ;;(set-CPS-variable :current-shared-goal content)
    ;;(set-CPS-variable :system-context context)
    (update-csm (list 'PRIVATE-SYSTEM-GOAL :content content :context context))
    (setf (user-time-of-last-interaction user) (get-time-of-day))
    (cache-response-for-processing `((CSM-RESPONSE XX PRIVATE-SYSTEM-GOAL :content ,content :context ,context)))
    (invoke-state 'initiate-CPS-goal user nil nil)))


;;;  Here's the code that manages the interface to the CSM

(defun update-csm (update)
  (send-msg `(REQUEST :content (UPDATE-CSM :content ,update))))

(defun query-csm (&key content)
  (send-and-wait `(REQUEST :content (QUERY-CSM :content ,content))))

(defun find-CSM-interps (&key sa what result context new-akrl-context)
  (let ((reply (send-and-wait `(REQUEST :content (INTERPRET-SPEECH-ACT :content (,sa :content ,what :context ,context)))))
	(result-value (find-arg reply :result))
	(new-akrl-context-value reply :new-context))
    (im::match-vals nil result result-value)
    (im::match-vals nil new-akrl-context new-akrl-context-value)))
