
(in-package :llearner)


(defstruct utt
  uttnum
  speech-act
  cps-act
  status
  interpretations
  words
  start
  end)

(defvar *w-package* (find-package :w))
(defvar *max-uttnum* 1000)
(defvar *record* (make-array *max-uttnum* :initial-element nil))

  
(defun initialize nil
  (trace-msg 3 "Language learner starting")
  nil)

(defun end-of-turn (&key uttnum words channel)
  (trace-msg 1 "received end of turn")
   (when (numberp uttnum)
     (let ((rec (aref *record* uttnum)))
       (if (eq (utt-status rec) 'success)
	   (language-learn (aref *record* uttnum))
			  
	   ))
     ))

(defun utt-failure (&key channel words code uttnum)
  (trace-msg 1 "received utt failure: ~S" uttnum)
  (if (numberp uttnum)
      (setf (utt-status (aref *record* uttnum)) 'failed)
      (format t "~%LLEARNER: failure msg without an uttnum")
    ))

(defun new-speech-act (args)
  (let* ((sa (car args))
	 (uttnum (if (eq (car sa) 'utt)
		    (find-arg-in-act sa :uttnum)
		    (find-arg-in-act (car (find-arg-in-act sa :acts)) :uttnum)))  ;; get UTTNUM from first SA in compound act
	)
    ;;(format t "IN NEW-SPEECH-ACT with uttnum ~S" uttnum)
    (if (numberp uttnum)
	(let ((rec (or (aref *record* uttnum)
		       (setf (aref *record* uttnum) (make-utt)))))
	  (setf (utt-speech-act rec) args)
	  (setf (utt-uttnum rec) uttnum)
	  (setf (utt-start rec) (find-arg-in-act sa :start))
	  (setf (utt-end rec) (find-arg-in-act sa :end)))
	(format t "LLEARNER WARNING: no uttnum found in ~S" args))
    ))
          
(defun interpretation (args)
  (let ((uttnum (find-arg-in-act args :uttnum))
	(act (car args))
	)
    (if (numberp uttnum)
	(let ((rec (aref *record* uttnum)))
	  (setf (utt-interpretations rec)
		(append (utt-interpretations rec) (list args)))
	  (setf (utt-status rec) 'success))
	(format t "LLEARNER WARNING: no uttnum found in ~S" args))
    ))

(defun language-learn (rec)
  "we have a successful interpretation that covers the utterance - we look for REFENTIAL-SEM words and try to refine the types"
  (let* ((sa (car (utt-speech-act rec)))
	 (satypecar sa)
	 (sa-start (find-arg-in-act sa :start))
	 (sa-end (find-arg-in-act sa :end))
	 (extractions (utt-interpretations rec))
	 (start-ends (remove-if #'null
				(mapcar #'get-start-end extractions)))
	 )
    (trace-msg 2 "Starting Language learning on utterance ~S" (utt-uttnum rec))
    (multiple-value-bind
	  (extract-start extract-end)
	(find-min-max-position start-ends)
      (when (and (numberp sa-start) (numberp sa-end) (numberp extract-start) (numberp extract-end))
	(let ((coverage (/ (- extract-end extract-start) (- sa-end sa-start))))
	  (format t "~% Coverage is ~S" coverage)
	  (when (> coverage .9)
	    (mapcar #'learn-from-term (find-terms (car (utt-speech-act rec))))))))))

(defun find-terms (sa)
  (case (car sa)
    (utt (find-arg-in-act sa :terms))
    (compound-communication-act 
     (let ((term-list (mapcar #'find-terms (find-arg-in-act sa :acts))))
       (apply #'append term-list)))))

(defun get-start-end (lf)
  (let ((start (find-arg-in-act lf :start))
	(end (find-arg-in-act lf :end)))
    (if (and start end)
	(list start end))))

(defun find-min-max-position (values)
  (if (cdr values)
      (multiple-value-bind (min max)
	  (find-min-max-position (cdr values))
	(values (min (caar values) min) (max (cadar values) max)))
      (values (caar values) (cadar values))))


(defvar *new-words* nil)
(defun remember-new-word (word cat)
  (push (list word cat) *new-words*)
  )
(defun not-seen-already (word cat)
  (not (find-if #'(lambda (x) (and (eq word (car x)) (eq cat (cadr x)))) *new-words*)))

(defun learn-from-term (term)
  (let* ((lf (find-arg-in-act term :lf))
	 (sem (find-arg-in-act term :sem)))
    (multiple-value-bind (type word)
	(analyze-type (third lf))
      (when word 
	(case (car lf)
	  ((ont::A ont::THE ont::THE-SET ont::INDEF-SET)
	   (when (and (eq type 'ont::referential-sem) (not-seen-already word 'N)
		      (not (member word '(W::thing w::one))))
	     (learn-new-lex-entry word 'N 'ont::PHYS-OBJECT sem)))
	  ((ont::F)
	   (cond 
	     ((and (eq type 'ont::modifier) (not-seen-already word 'ADJ))
	      (learn-new-lex-entry word 'ADJ 'ont::modifier sem))
	     #||((and (eq type 'ont::situation-root) (not-seen-already word 'V))
	      (learn-new-lex-entry word 'V 'ont::situation-root sem))))||#
	     ))
	  (otherwise nil))))))

(defun identify-likely-type (default sem)
  default)

(defun learn-new-lex-entry (word pos defaulttype sem)
  (remember-new-word word pos)
  (send-msg `(request :content (add-lex-entry :word ,word :cat ,(convert-to-package pos *w-package*) :type ,(identify-likely-type defaulttype sem)))))

(defun analyze-type (type)
  (if (consp type)
      (values (second type) (third type)
      (values nil type))))

(defun interp-cps-act-hyps (args)
   (Trace-msg 1 "received ~S" args)
   (let ((uttnum (find-arg args :uttnum)))
     (if (numberp uttnum)
	 (setf (utt-cps-act (aref *record* uttnum)) args)
	 (format t "LLEARNER WARNING: no uttnum found in ~S" args))
     ))
