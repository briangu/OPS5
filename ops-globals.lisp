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

(in-package "OPS")

;;; Global variables also used by OPS modules.

(defvar *halt-flag*)
(defvar *cycle-count*)
(defvar *p-name*)
(defvar *ptrace*)
(defvar *wtrace*)

;;; Hash Tables.

(defvar *conflicts-table* (make-hash-table))

(defvar *vector-attribute-table* (make-hash-table))
(defun set-vector-attribute (att)
  (setf (gethash att *vector-attribute-table*) t))
(defun is-vector-attribute (att)
  (gethash att *vector-attribute-table*))

(defvar *att-list-table* (make-hash-table))
(defvar *ppdat-table* (make-hash-table))
(defvar *wmpart*-table* (make-hash-table))
(defvar *inputfile-table* (make-hash-table))
(defvar *outputfile-table* (make-hash-table))
(defvar *backpointers-table* (make-hash-table))
(defvar *ops-bind-table* (make-hash-table))
(defvar *production-table* (make-hash-table))
(defvar *topnode-table* (make-hash-table))
(defvar *external-routine-table* (make-hash-table))

(defun clear-ops-hash-tables ()
  (clrhash *conflicts-table*)
  (clrhash *vector-attribute-table*)
  (clrhash *att-list-table*)
  (clrhash *ppdat-table*)
  (clrhash *wmpart*-table*)
  (clrhash *inputfile-table*)
  (clrhash *outputfile-table*)
  (clrhash *backpointers-table*)
  (clrhash *ops-bind-table*)
  (clrhash *production-table*)
  (clrhash *topnode-table*)
  (clrhash *external-routine-table*))

