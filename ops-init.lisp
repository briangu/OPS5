;;; ****************************************************************
;;; OPS5 Interpreter ***********************************************
;;; ****************************************************************
;;; This Common Lisp version of OPS5 is in the public domain.  It is based
;;; in part on based on a Franz Lisp implementation done by Charles L. Forgy
;;; at Carnegie-Mellon University, which was placed in the public domain by
;;; the author in accordance with CMU policies.  Ported to Common Lisp by 
;;; George Wood and Jim Kowalski. CMU Common Lisp modifications by
;;; Dario Guise, Skef Wholey, Michael Parzen, and Dan Kuokka. 
;;; Modified to work in CLtL1, CLtL2 and X3J13 compatible lisps by 
;;; Mark Kantrowitz on 14-OCT-92.
;;; 
;;; This code is made available is, and without warranty of any kind by the
;;; authors or by Carnegie-Mellon University.
;;;

;;; 15-OCT-92 mk    Modified definition of RESET-OPS.

(in-package "OPS")

(defparameter *ops-version* "19-OCT-92")

(defun ops-init ()
  ; Allows ^ , { , and } operators to be right next to another symbol.
  (set-macro-character #\{ #'(lambda (s c)
			       (declare (ignore s c))
			       '\{))
  (set-macro-character #\} #'(lambda (s c)
			       (declare (ignore s c))
			       '\}))
  (set-macro-character #\^ #'(lambda (s c)
			       (declare (ignore s c))
			       '\^))
  (backup-init)
  (compile-init)
  (main-init)
  (match-init)
  (io-init)
  (rhs-init)
  (format t "~&Common Lisp OPS5 interpreter, version ~A.~&"
	  *ops-version*))

(defun reset-ops ()
  "Clears the state of OPS to allow a new rule set to be loaded."

  ;; Tell the user what we're doing.
  (format t "~&Resetting OPS5 interpreter: ~
             ~&   deleting productions, working memory, etc.")
  (remove *)
  (ops-init)
  (clear-ops-hash-tables)
  ;; (i-g-v)
  (setq *class-list* nil
	*pcount* 0))

;;; *EOF*

