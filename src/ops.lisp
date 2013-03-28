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

;;; Change Log:
;;; 13-OCT-92 mk    Modified pathname usage to not be CMU Common Lisp specific.
;;; 15-OCT-92 mk    Changed definition of // in ARI so that division uses /
;;;                 instead of FLOOR. Added QUOTIENT infix operator for
;;;                 backward compatability.
;;; 15-OCT-92 mk    Moved all exports and shadows to this file, and modified
;;;                 them somewhat, to allow OPS5 to run on the Macintosh.

;;;; This file handles the loading of all files composing the OPS interpreter.
;;;; It also performs the necessary initialization.

(in-package :cl-user)

(eval-when (compile load eval)
  #+(or (and :excl (or :allegro-v4.0 :allegro-v4.1)) :mcl)
  (pushnew :cltl2 *features*))

(defpackage "OPS" (:use "COMMON-LISP")
  (:shadow "REMOVE" "WRITE")
  (:export "REMOVE" "WRITE" "MAKE" "MODIFY" "CRLF"
	   "-->"
	   "LITERALIZE" "P" "VECTOR-ATTRIBUTE" "STRATEGY" "WATCH" "RESET-OPS"))

(in-package "OPS")

;;; ********************************
;;; Source Directory ***************
;;; ********************************

(defparameter *ops-code-directory* #.(or *compile-file-pathname*
                                         *load-pathname*)
              "Directory where OPS5 source code is stored.")

;;; ********************************
;;; REP Loop ***********************
;;; ********************************

(defun ops ()
  "OPS Read-Eval-Print Loop."
  (let ((counter 0))
    (loop
      (incf counter)
      (format t "~&~D. " counter)
      (let* ((input (string-trim '(#\space #\tab) (read-line)))
	     (space-pos (position #\space input))
	     (tag (subseq input 0 space-pos)))
	(cond ((or (string-equal tag "exit")
		   (string-equal tag "quit"))
	       (return))
	      ((string-equal tag "load")
	       (load (subseq input (1+ space-pos))))
	      (t
	       (let ((form (read-from-string (concatenate 'string
							  "(" input ")"))))
		 (print (eval form)))))))))

;;; *EOF*

