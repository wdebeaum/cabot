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

(defun find-CSM-interps (&key sa what result context new-akrl-context test active-goal)
  (let* ((speechact (if test
		       `(INTERPRET-SPEECH-ACT :content (,sa :content ,what :context ,context :test ,test :active-goal ,active-goal))
		       `(INTERPRET-SPEECH-ACT :content (,sa :content ,what :context ,context :active-goal ,active-goal))))
	 (reply (send-and-wait `(REQUEST :content ,speechact)))
	 (result-value (find-arg-in-act reply :content))
	 (new-akrl-context-value (find-arg-in-act reply :context)))
    (append (im::match-vals nil result result-value)
	    (im::match-vals nil new-akrl-context new-akrl-context-value))))

	
(defun take-initiative? (&key result goal context)
  (let* ((reply (send-and-wait `(REQUEST :content (take-initiative? :goal ,goal :context ,context))))
	 (result-value (find-arg-in-act reply :result))
	 
	 (new-akrl-context-value (find-arg reply :context)))
    ;;(format t "%% Take INIT RETURNS ~S" reply)
    reply))
