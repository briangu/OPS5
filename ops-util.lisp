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

;;;; This file contains utility definitions that are needed by other ops
;;;; modules.  This must be loaded first so commonlisp systems that
;;;; expand macros early have them available.

;;; Change Log:
;;; 13-OCT-92 mk    Replaced all uses of ASSQ with ASSOC, as appropriate.
;;; 13-OCT-92 mk    Replaced all uses of DELQ with DELETE #'EQ.
;;; 13-OCT-92 mk    Renamed SP-DELETE as TREE-REMOVE and modified the
;;;                 definition slightly.
;;; 13-OCT-92 mk    Got rid of PUTVECTOR and GETVECTOR.
;;; 13-OCT-92 mk    Eliminated usage of PUTPROP, GET, and REMPROP.
;;; 13-OCT-92 mk    Replaced CE-GELM with a call to NTH.
;;; 13-OCT-92 mk    Replaced INTERQ with INTERSECTION.
;;; 13-OCT-92 mk    Replaced FIX with FLOOR.
;;; 13-OCT-92 mk    Replaced NCONS with LIST.
;;; 13-OCT-92 mk    Replaced DTPR with CONSP.


(in-package "OPS")


(defun tree-remove (element tree &key (test #'equal))
  "TREE-REMOVE is a function which deletes every occurence
   of ELEMENT from TREE. This function was defined because Common Lisp's
   REMOVE function only removes top level elements from a list."
  (when tree
    (if (funcall test element (car tree)) 
	(tree-remove element (cdr tree) :test test)
	(cons (car tree)
	      (tree-remove element (cdr tree) :test test)))))

;;; Functions that were revised so that they would compile efficiently
(eval-when (compile eval load)

(defmacro == (x y)
  ;; Skef Wholey - The = function in Common Lisp will compile into good code
  ;; (in all implementations that I know of) when given the right declarations.
  ;; In this case, we know both numbers are fixnums, so we use that 
  ;; information.
  `(= (the fixnum ,x) (the fixnum ,y)))

(defmacro =alg (a b)
  ;; =ALG returns T if A and B are algebraically equal.
  ;; This corresponds to equalp - Dario Giuse
  ;; But equalp uses eql for comparison if the things are numbers - Skef Wholey
  `(eql ,a ,b))

(defmacro fast-symeval (&body z)
  `(symbol-value ,(car z)))

) ;eval-when
 

; The loops in gelm were unwound so that fewer calls on DIFFERENCE
; would be needed

(defun gelm (x k)
  ; (locally) 				;@@@ locally isn't implemented yet
  (declare (optimize speed))
  (prog (ce sub)
    (setq ce (truncate  k 10000.))		;use multiple-value-setq???
    (setq sub (- k (* ce 10000.)))		;@@@ ^
    
    celoop (and (eq ce 0.) (go ph2))
    (setq x (cdr x))
    (and (eq ce 1.) (go ph2))
    (setq x (cdr x))
    (and (eq ce 2.) (go ph2))
    (setq x (cdr x))
    (and (eq ce 3.) (go ph2))
    (setq x (cdr x))
    (and (eq ce 4.) (go ph2))
    (setq ce (- ce 4.))
    (go celoop)
    ph2  (setq x (car x))
    subloop (and (eq sub 0.) (go finis))
    (setq x (cdr x))
    (and (eq sub 1.) (go finis))
    (setq x (cdr x))
    (and (eq sub 2.) (go finis))
    (setq x (cdr x))
    (and (eq sub 3.) (go finis))
    (setq x (cdr x))
    (and (eq sub 4.) (go finis))
    (setq x (cdr x))
    (and (eq sub 5.) (go finis))
    (setq x (cdr x))
    (and (eq sub 6.) (go finis))
    (setq x (cdr x))
    (and (eq sub 7.) (go finis))
    (setq x (cdr x))
    (and (eq sub 8.) (go finis))
    (setq sub (- sub 8.))
    (go subloop)
    finis (return (car x))) ) ;  )  	;end prog,< locally >, defun

(defun %warn (what where)
  (format t "~&?~@[~A~]..~A..~A"
	   *p-name* where what)
  where) 

(defun %error (what where)
  (%warn what where)
  (throw '!error! '!error!)) 	;jgk quoted arguments

(defun top-levels-eq (la lb)
  (do ((sublist-a la (cdr sublist-a))
       (sublist-b lb (cdr sublist-b)))
      ((eq sublist-a sublist-b)
       t)
    (when (or (null sublist-a)
	      (null sublist-b)
	      (not (eq (car sublist-a) (car sublist-b))))
      (return nil)))
  #|(prog nil
    lx   (cond ((eq la lb) (return t))
	       ((null la) (return nil))
	       ((null lb) (return nil))
	       ((not (eq (car la) (car lb))) (return nil)))
    (setq la (cdr la))
    (setq lb (cdr lb))
    (go lx))|#
  ) 

;@@@ revision suggested by sf/inc. by gdw
(defun variablep (x)
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\< )))


#|
Commented out - Dario Giuse.
This is unnecessary in Spice Lisp

; break mechanism:
(proclaim '(special erm *break-character*))

(defun setbreak nil (setq *break-flag* t))
(setq *break-character* #\control-D)
(bind-keyboard-function *break-character* #'setbreak)
(princ "*** use control-d for ops break, or setq *break-character asciival***")

|#
;;; *EOF*
