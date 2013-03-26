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

;;;; This file contains functions compile productions.

(in-package "OPS")
;(shadow '(remove write))    ; Should get this by requiring ops-rhs
;(export '--> )


;;; External global variables

(defvar *real-cnt* nil)
(defvar *virtual-cnt* nil)
(defvar *last-node* nil)
(defvar *first-node* nil)
(defvar *pcount* nil)


;;; Internal global variables

(defvar *matrix* nil)
(defvar *curcond* nil)
(defvar *feature-count* nil)
(defvar *ce-count* nil)
(defvar *vars* nil)
(defvar *ce-vars* nil)
(defvar *rhs-bound-vars* nil)
(defvar *rhs-bound-ce-vars* nil)
(defvar *last-branch* nil)
(defvar *subnum* nil)
(defvar *cur-vars* nil)
(defvar *action-type* nil)



(defun compile-init ()
  (setq *real-cnt* (setq *virtual-cnt* 0.))
  (setq *pcount* 0.)
  (make-bottom-node))


;;; LHS Compiler

(defun ops-p (z) 
  (finish-literalize)
  (princ '*) 
  ;(drain) commented out temporarily
  (force-output)			;@@@ clisp drain?
  (compile-production (car z) (cdr z))) 


(defun compile-production (name matrix)
  ;; jgk inverted args to catch and quoted tag
  (setq *p-name* name)
  (catch '!error! (cmp-p name matrix))
  (setq *p-name* nil))
#|
(defun compile-production (name matrix) ;jgk inverted args to catch 
  (prog (erm)				;and quoted tag
    (setq *p-name* name)
    (setq erm (catch '!error! (cmp-p name matrix)))
    (setq *p-name* nil)))
|#

(defun peek-lex ()
  (car *matrix*)) 

(defun lex ()
  (pop *matrix*)) 

(defun end-of-p () (atom *matrix*)) 

(defun rest-of-p () *matrix*) 

(defun prepare-lex (prod) (setq *matrix* prod)) 


(defun peek-sublex () (car *curcond*)) 

(defun sublex ()
  (pop *curcond*)) 

(defun end-of-ce () (atom *curcond*)) 

(defun rest-of-ce () *curcond*) 

(defun prepare-sublex (ce) (setq *curcond* ce)) 

(defun make-bottom-node ()
  (setq *first-node* (list '&bus nil))) 

(defun cmp-p (name matrix)
  (prog (m bakptrs)
    (cond ((or (null name) (consp  name))	;dtpr\consp gdw
	   (%error '|illegal production name| name))
	  ((equal (gethash name *production-table*) matrix)
	   (return nil)))
    (prepare-lex matrix)
    (excise-p name)
    (setq bakptrs nil)
    (incf *pcount*)		;"plus" changed to "+" by gdw
    (setq *feature-count* 0.)
    (setq *ce-count* 0)
    (setq *vars* nil)
    (setq *ce-vars* nil)
    (setq *rhs-bound-vars* nil)
    (setq *rhs-bound-ce-vars* nil)
    (setq *last-branch* nil)
    (setq m (rest-of-p))
    l1   (and (end-of-p) (%error '|no '-->' in production| m))
    (cmp-prin)
    (setq bakptrs (cons *last-branch* bakptrs))
    (or (eq '--> (peek-lex)) (go l1))
    (lex)
    (check-rhs (rest-of-p))
    (link-new-node (list '&p
			 *feature-count*
			 name
			 (encode-dope)
			 (encode-ce-dope)
			 (cons 'progn (rest-of-p))))
    (setf (gethash name *backpointers-table*) (cdr (nreverse bakptrs)))
    (setf (gethash name *production-table*) matrix)
    (setf (gethash name *topnode-table*) *last-node*))) 

(defun rating-part (pnode) (cadr pnode)) 

(defun var-part (pnode) (car (cdddr pnode))) 

(defun ce-var-part (pnode) (cadr (cdddr pnode))) 

(defun rhs-part (pnode) (caddr (cdddr pnode))) 

(defun cmp-prin nil
  (setq *last-node* *first-node*)
  (cond ((null *last-branch*) (cmp-posce) (cmp-nobeta))
	((eq (peek-lex) '-) (cmp-negce) (cmp-not))
	(t (cmp-posce) (cmp-and)))) 

(defun cmp-negce nil (lex) (cmp-ce)) 

(defun cmp-posce nil
  (setq *ce-count* (1+ *ce-count*))		;"plus" changed to "+" by gdw
  (cond ((eq (peek-lex) '\{) (cmp-ce+cevar))	;"plus" changed to "+" by gdw
	(t (cmp-ce)))) 

(defun cmp-ce+cevar ()
  (prog (z)
    (lex)
    (cond ((atom (peek-lex)) (cmp-cevar) (cmp-ce))
	  (t (cmp-ce) (cmp-cevar)))
    (setq z (lex))
    (or (eq z '\}) (%error '|missing '}'| z)))) 

(defun new-subnum (k)
  (or (numberp k) (%error '|tab must be a number| k))
  (setq *subnum* (floor k))) 

(defun incr-subnum ()
  (incf *subnum*)) 

(defun cmp-ce ()
  (prog (z)
    (new-subnum 0.)
    (setq *cur-vars* nil)
    (setq z (lex))
    (and (atom z)
	 (%error '|atomic conditions are not allowed| z))
    (prepare-sublex z)
    la   (and (end-of-ce) (return nil))
    (incr-subnum)
    (cmp-element)
    (go la))) 

(defun cmp-element nil
  (when (eq (peek-sublex) '^)
    (cmp-tab))
  (cond ((eq (peek-sublex) '\{) (cmp-product))
	(t (cmp-atomic-or-any))))

(defun cmp-atomic-or-any ()
  (cond ((eq (peek-sublex) '<<) (cmp-any))
	(t (cmp-atomic))))

(defun cmp-any ()
  (prog (a z)
    (sublex)
    (setq z nil)
    la   (cond ((end-of-ce) (%error '|missing '>>'| a)))
    (setq a (sublex))
    (cond ((not (eq '>> a)) (setq z (cons a z)) (go la)))
    (link-new-node (list '&any nil (current-field) z)))) 

(defun cmp-tab nil
  (prog (r)
    (sublex)
    (setq r (sublex))
    (setq r ($litbind r))
    (new-subnum r))) 

(defun get-bind (x)
  (when (symbolp x)
    (literal-binding-of x))) 

(defun cmp-atomic nil
  (prog (test x)
    (setq x (peek-sublex))
    (cond ((eq x '= ) (setq test 'eq) (sublex))
	  ((eq x '<>) (setq test 'ne) (sublex))
	  ((eq x '<) (setq test 'lt) (sublex))
	  ((eq x '<=) (setq test 'le) (sublex))
	  ((eq x '>) (setq test 'gt) (sublex))
	  ((eq x '>=) (setq test 'ge) (sublex))
	  ((eq x '<=>) (setq test 'xx) (sublex))
	  (t (setq test 'eq)))
    (cmp-symbol test))) 

(defun cmp-product ()
  (prog (save)
    (setq save (rest-of-ce))
    (sublex)
    la   (cond ((end-of-ce)
		(cond ((member '\} save :test #'equal) 
		       (%error '|wrong contex for '}'| save))
		      (t (%error '|missing '}'| save))))
	       ((eq (peek-sublex) '\}) (sublex) (return nil)))
    (cmp-atomic-or-any)
    (go la))) 

(defun cmp-symbol (test)
  (let ((flag t))
    (when (eq (peek-sublex) '//)
      (sublex)
      (setq flag nil))
    (cond ((and flag (variablep (peek-sublex)))
	   (cmp-var test))
	  ((numberp (peek-sublex)) (cmp-number test))
	  ((symbolp (peek-sublex)) (cmp-constant test))
	  (t (%error '|unrecognized symbol| (sublex)))))) 

(defun cmp-constant (test)   ;jgk inserted concatenate form
  (or (member test '(eq ne xx))
      (%error '|non-numeric constant after numeric predicate| (sublex)))
  (link-new-node (list (intern (concatenate 'string
					    "T"
					    (symbol-name  test)
					    "A"))
		       nil
		       (current-field)
		       (sublex)))) 

(defun cmp-number (test)		;jgk inserted concatenate form
  (link-new-node (list (intern (concatenate 'string
					    "T"
					    (symbol-name  test)
;@@@ error? reported by laird fix\	    "A"
					    "N"))
		       nil
		       (current-field)
		       (sublex)))) 

(defun current-field () (field-name *subnum*)) 

(defun field-name (num)
  (if (< 0 num 127)
      (svref '#(nil *c1* *c2* *c3* *c4* *c5* *c6* *c7* *c8* *c9* *c10* *c11*
		    *c12* *c13* *c14* *c15* *c16* *c17* *c18* *c19* *c20* *c21*
		    *c22* *c23* *c24* *c25* *c26* *c27* *c28* *c29* *c30* *c31*
		    *c32* *c33* *c34* *c35* *c36* *c37* *c38* *c39* *c40* *c41*
		    *c42* *c43* *c44* *c45* *c46* *c47* *c48* *c49* *c50* *c51*
		    *c52* *c53* *c54* *c55* *c56* *c57* *c58* *c59* *c60* *c61*
		    *c62* *c63* *c64* *c65* *c66* *c67* *c68* *c69* *c70* *c71*
		    *c72* *c73* *c74* *c75* *c76* *c77* *c78* *c79* *c80* *c81*
		    *c82* *c83* *c84* *c85* *c86* *c87* *c88* *c89* *c90* *c91*
		    *c92* *c93* *c94* *c95* *c96* *c97* *c98* *c99* *c100*
		    *c101* *c102* *c103* *c104* *c105* *c106* *c107* *c108*
		    *c109* *c110* *c111* *c112* *c113* *c114* *c115* *c116*
		    *c117* *c118* *c119* *c120* *c121* *c122* *c123* *c124*
		    *c125* *c126* *c127*)
	     num)
      (%error '|condition is too long| (rest-of-ce))))

;;; Compiling variables
;
;
;
; *cur-vars* are the variables in the condition element currently 
; being compiled.  *vars* are the variables in the earlier condition
; elements.  *ce-vars* are the condition element variables.  note
; that the interpreter will not confuse condition element and regular
; variables even if they have the same name.
;
; *cur-vars* is a list of triples: (name predicate subelement-number)
; eg:		( (<x> eq 3)
;		  (<y> ne 1)
;		  . . . )
;
; *vars* is a list of triples: (name ce-number subelement-number)
; eg:		( (<x> 3 3)
;		  (<y> 1 1)
;		  . . . )
;
; *ce-vars* is a list of pairs: (name ce-number)
; eg:		( (ce1 1)
;		  (<c3> 3)
;		  . . . )

;;; used only in this file.
(defmacro var-dope (var) `(assoc ,var *vars*))

(defmacro ce-var-dope (var) `(assoc ,var *ce-vars*))

(defun cmp-var (test)
  (let* ((name (sublex))
	 (old (assoc name *cur-vars*)))
    (cond ((and old (eq (cadr old) 'eq))
	   (cmp-old-eq-var test old))
	  ((and old (eq test 'eq)) (cmp-new-eq-var name old))
	  (t (cmp-new-var name test))))) 

(defun cmp-new-var (name test)
  (push (list name test *subnum*) 
	*cur-vars*)) 

(defun cmp-old-eq-var (test old)	; jgk inserted concatenate form
  (link-new-node (list (intern (concatenate 'string
					    "T"
					    (symbol-name  test)
					    "S"))
		       nil
		       (current-field)
		       (field-name (caddr old))))) 

(defun cmp-new-eq-var (name old)	;jgk inserted concatenate form
  (prog (pred next)
    (setq *cur-vars* (delete old *cur-vars* :test #'eq))
    (setq next (assoc name *cur-vars*))
    (cond (next (cmp-new-eq-var name next))
	  (t (cmp-new-var name 'eq)))
    (setq pred (cadr old))
    (link-new-node (list (intern (concatenate 'string
					      "T"
					      (symbol-name  pred)
					      "S"))
			 nil
			 (field-name (caddr old))
			 (current-field))))) 

(defun cmp-cevar nil
  (let* ((name (lex))
	 (old (assoc name *ce-vars*)))
    (when old
      (%error '|condition element variable used twice| name))
    (push (list name 0.) 
	  *ce-vars*))) 

(defun cmp-not nil (cmp-beta '&not)) 

(defun cmp-nobeta nil (cmp-beta nil)) 

(defun cmp-and nil (cmp-beta '&and)) 

(defun cmp-beta (kind)
  (prog (tlist vdope vname #|vpred vpos|# old)
    (setq tlist nil)
    la   (and (atom *cur-vars*) (go lb))
    (setq vdope (car *cur-vars*))
    (setq *cur-vars* (cdr *cur-vars*))
    (setq vname (car vdope))
    ;;  (setq vpred (cadr vdope))    Dario - commented out (unused)
    ;;  (setq vpos (caddr vdope))
    (setq old (assoc vname *vars*))
    (cond (old (setq tlist (add-test tlist vdope old)))
	  ((not (eq kind '&not)) (promote-var vdope)))
    (go la)
    lb   (and kind (build-beta kind tlist))
    (or (eq kind '&not) (fudge))
    (setq *last-branch* *last-node*))) 

(defun add-test (list new old) ; jgk inserted concatenate form
  (prog (ttype lloc rloc)
    (incf *feature-count*)
    (setq ttype (intern (concatenate 'string "T"
				     (symbol-name (cadr new))
				     "B")))
    (setq rloc (encode-singleton (caddr new)))
    (setq lloc (encode-pair (cadr old) (caddr old)))
    (return (cons ttype (cons lloc (cons rloc list)))))) 

; the following two functions encode indices so that gelm can
; decode them as fast as possible

(defun encode-pair (a b) (+ (* 10000. (1- a)) (1- b))) 
;"plus" changed to "+" by gdw

(defun encode-singleton (a) (1- a)) 

(defun promote-var (dope)
  (prog (vname vpred vpos new)
    (setq vname (car dope))
    (setq vpred (cadr dope))
    (setq vpos (caddr dope))
    (or (eq 'eq vpred)
	(%error '|illegal predicate for first occurrence|
		(list vname vpred)))
    (setq new (list vname 0. vpos))
    (setq *vars* (cons new *vars*)))) 

(defun fudge nil
  (mapc #'fudge* *vars*)
  (mapc #'fudge* *ce-vars*)) 

(defun fudge* (z)
  (let ((a (cdr z)))
    (incf (car a)))) 

(defun build-beta (type tests)
  (prog (rpred lpred lnode lef)
    (link-new-node (list '&mem nil nil (protomem)))
    (setq rpred *last-node*)
    (cond ((eq type '&and)
	   (setq lnode (list '&mem nil nil (protomem))))
	  (t (setq lnode (list '&two nil nil))))
    (setq lpred (link-to-branch lnode))
    (cond ((eq type '&and) (setq lef lpred))
	  (t (setq lef (protomem))))
    (link-new-beta-node (list type nil lef rpred tests)))) 

(defun protomem nil (list nil)) 

(defun memory-part (mem-node) (car (cadddr mem-node))) 

(defun encode-dope nil
  (prog (r all z k)
    (setq r nil)
    (setq all *vars*)
    la   (and (atom all) (return r))
    (setq z (car all))
    (setq all (cdr all))
    (setq k (encode-pair (cadr z) (caddr z)))
    (setq r (cons (car z) (cons k r)))
    (go la))) 

(defun encode-ce-dope nil
  (prog (r all z k)
    (setq r nil)
    (setq all *ce-vars*)
    la   (and (atom all) (return r))
    (setq z (car all))
    (setq all (cdr all))
    (setq k (cadr z))
    (setq r (cons (car z) (cons k r)))
    (go la))) 



;;; Linking the nodes

(defun link-new-node (r)
  (cond ((not (member (car r) '(&p &mem &two &and &not) :test #'equal))
	 (setq *feature-count* (1+ *feature-count*))))
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-node* (link-left *last-node* r))) 

(defun link-to-branch (r)
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-branch* (link-left *last-branch* r))) 

(defun link-new-beta-node (r)
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-node* (link-both *last-branch* *last-node* r))
  (setq *last-branch* *last-node*)) 

(defun link-left (pred succ)
  (prog (a r)
    (setq a (left-outs pred))
    (setq r (find-equiv-node succ a))
    (and r (return r))
    (setq *real-cnt* (1+ *real-cnt*))
    (attach-left pred succ)
    (return succ))) 

(defun link-both (left right succ)
  (prog (a r)
    (setq a (intersection (left-outs left) (right-outs right)))
    (setq r (find-equiv-beta-node succ a))
    (and r (return r))
    (setq *real-cnt* (1+ *real-cnt*))
    (attach-left left succ)
    (attach-right right succ)
    (return succ))) 

(defun attach-right (old new)
  (rplaca (cddr old) (cons new (caddr old)))) 

(defun attach-left (old new)
  (rplaca (cdr old) (cons new (cadr old)))) 

(defun right-outs (node) (caddr node)) 

(defun left-outs (node) (cadr node)) 

(defun find-equiv-node (node list)
  (prog (a)
    (setq a list)
    l1   (cond ((atom a) (return nil))
	       ((equiv node (car a)) (return (car a))))
    (setq a (cdr a))
    (go l1))) 

(defun find-equiv-beta-node (node list)
  (prog (a)
    (setq a list)
    l1   (cond ((atom a) (return nil))
	       ((beta-equiv node (car a)) (return (car a))))
    (setq a (cdr a))
    (go l1))) 

; do not look at the predecessor fields of beta nodes; they have to be
; identical because of the way the candidate nodes were found

(defun equiv (a b)
  (and (eq (car a) (car b))
       (or (eq (car a) '&mem)
	   (eq (car a) '&two)
	   (equal (caddr a) (caddr b)))
       (equal (cdddr a) (cdddr b)))) 

(defun beta-equiv (a b)
  (and (eq (car a) (car b))
       (equal (cddddr a) (cddddr b))
       (or (eq (car a) '&and) (equal (caddr a) (caddr b))))) 

; the equivalence tests are set up to consider the contents of
; node memories, so they are ready for the build action



;;; Check the RHSs of productions 


(defun check-rhs (rhs) (mapc #'check-action rhs))

(defun check-action (x)
  (if (atom x)
      (%warn '|atomic action| x)
      (let ((a (car x)))
	(setq *action-type* a)
	(case a
	  (bind (check-bind x))
	  (cbind (check-cbind x))
	  (make (check-make x))
	  (modify (check-modify x))
	  (remove (check-remove x))
	  (write (check-write x))	
	  (call (check-call x))		
	  (halt (check-halt x))
	  (openfile (check-openfile x))
	  (closefile (check-closefile x))
	  (default (check-default x))
	  (build (check-build x))
	  (t (%warn '|undefined rhs action| a))))))


;(defun chg-to-write (x)
;	(setq x (cons 'write (cdr x))))

(defun check-build (z)
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (check-build-collect (cdr z)))

(defun check-build-collect (args)
  (prog (r)
    top	(and (null args) (return nil))
    (setq r (car args))
    (setq args (cdr args))
    (cond ((consp  r) (check-build-collect r))	;dtpr\consp gdw
	  ((eq r '\\)
	   (and (null args) (%warn '|nothing to evaluate| r))
	   (check-rhs-value (car args))
	   (setq args (cdr args))))
    (go top)))

(defun check-remove (z) 				;@@@ kluge by gdw
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (mapc (function check-rhs-ce-var) (cdr z))) 

;(defun check-remove (z) 					;original
   ; (and (null (cdr z)) (%warn '|needs arguments| z))
   ;(mapc (function check-rhs-ce-var) (cdr z))) 

(defun check-make (z)
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-openfile (z)
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-closefile (z)
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-default (z)
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-modify (z)
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (check-rhs-ce-var (cadr z))
  (when (null (cddr z))
    (%warn '|no changes to make| z))
  (check-change& (cddr z))) 

(defun check-write (z)				;note this works w/write
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-call (z)
  (when (null (cdr z))
    (%warn '|needs arguments| z))
  (let ((f (cadr z)))
    (when (variablep f)
      (%warn '|function name must be a constant| z))
    (unless (symbolp f)
      (%warn '|function name must be a symbolic atom| f))
    (unless (externalp f)
      (%warn '|function name not declared external| f))
    (check-change& (cddr z)))) 

(defun check-halt (z)
  (unless (null (cdr z))
    (%warn '|does not take arguments| z))) 

(defun check-cbind (z)
  (unless (= (length z) 2.)
    (%warn '|takes only one argument| z))
  (let ((v (cadr z)))
    (unless (variablep v)
      (%warn '|takes variable as argument| z))
    (note-ce-variable v))) 

(defun check-bind (z)
  (unless (> (length z) 1.)
    (%warn '|needs arguments| z))
  (let ((v (cadr z)))
    (unless (variablep v)
      (%warn '|takes variable as argument| z))
    (note-variable v)
    (check-change& (cddr z)))) 

(defun check-change& (z)
  (prog (r tab-flag)
    (setq tab-flag nil)
    la   (and (atom z) (return nil))
    (setq r (car z))
    (setq z (cdr z))
    (cond ((eq r '^)
	   (and tab-flag
		(%warn '|no value before this tab| (car z)))
	   (setq tab-flag t)
	   (check-tab-index (car z))
	   (setq z (cdr z)))
	  ((eq r '//) (setq tab-flag nil) (setq z (cdr z)))
	  (t (setq tab-flag nil) (check-rhs-value r)))
    (go la))) 

(defun check-rhs-ce-var (v)
  (cond ((and (not (numberp v)) (not (ce-bound? v)))
	 (%warn '|unbound element variable| v))
	((and (numberp v) (or (< v 1.) (> v *ce-count*)))
	 (%warn '|numeric element designator out of bounds| v)))) 

(defun check-rhs-value (x)
  (if (consp x)				;dtpr\consp gdw 
      (check-rhs-function x)
      (check-rhs-atomic x))) 

(defun check-rhs-atomic (x)
  (when (and (variablep x) 
	     (not (bound? x)))
    (%warn '|unbound variable| x)))

(defun check-rhs-function (x)
  (let ((a (car x)))
    (case a
      (compute (check-compute x))
      (arith (check-compute x))
      (substr (check-substr x))
      (accept (check-accept x))
      (acceptline (check-acceptline x))
      (crlf (check-crlf x))
      (genatom (check-genatom x))
      (litval (check-litval x))
      (tabto (check-tabto x))
      (rjust (check-rjust x))
      (otherwise 
       (when (not (externalp a))
	 (%warn '"rhs function not declared external" a))))))

(defun externalp (x)
  ;  (cond ((symbolp x) (gethash x *external-routine-table*)) 	;) @@@
  ;ok, I'm eliminating this temporarily @@@@
  (cond ((symbolp x) t)
	(t (%warn '|not a legal function name| x) nil)))

(defun check-litval (x) 
  (unless (= (length x) 2)
    (%warn '|wrong number of arguments| x))
  (check-rhs-atomic (cadr x)))

(defun check-accept (x)
  (cond ((= (length x) 1) nil)
	((= (length x) 2) (check-rhs-atomic (cadr x)))
	(t (%warn '|too many arguments| x))))

(defun check-acceptline (x)
  (mapc #'check-rhs-atomic (cdr x)))

(defun check-crlf (x) 
  (check-0-args x)) 

(defun check-genatom (x) (check-0-args x)) 

(defun check-tabto (x)
  (unless (= (length x) 2)
    (%warn '|wrong number of arguments| x))
  (check-print-control (cadr x)))

(defun check-rjust (x)
  (unless (= (length x) 2)
    (%warn '|wrong number of arguments| x))
  (check-print-control (cadr x)))

(defun check-0-args (x)
  (unless (= (length x) 1.)
    (%warn '|should not have arguments| x))) 

(defun check-substr (x)
  (unless (= (length x) 4.)
    (%warn '|wrong number of arguments| x))
  (check-rhs-ce-var (cadr x))
  (check-substr-index (caddr x))
  (check-last-substr-index (cadddr x))) 

(defun check-compute (x) (check-arithmetic (cdr x))) 

(defun check-arithmetic (l)
  (cond ((atom l)
	 (%warn '|syntax error in arithmetic expression| l))
	((atom (cdr l)) (check-term (car l)))
	;; "plus" changed to "+" by gdw 
	;; "quotient" added by mk, for backward compatability with the
	;; old definition of //.
	((not (member (cadr l) '(+ - * // \\ quotient)))	
	 (%warn '|unknown operator| l))
	(t (check-term (car l)) (check-arithmetic (cddr l))))) 

(defun check-term (x)
  (if (consp x)				;dtpr\consp gdw
      (check-arithmetic x)
      (check-rhs-atomic x))) 

(defun check-last-substr-index (x)
  (or (eq x 'inf) (check-substr-index x))) 

(defun check-substr-index (x)
  (if (bound? x) x
      (let ((v ($litbind x)))
	(cond ((not (numberp v))
	       (%warn '|unbound symbol used as index in substr| x))
	      ((or (< v 1.) (> v 127.))
	       (%warn '|index out of bounds in tab| x)))))) 

(defun check-print-control (x)
  (cond ((bound? x) x)
	((or (not (numberp x)) (< x 1.) (> x 127.))
	 (%warn '|illegal value for printer control| x)))) 

(defun check-tab-index (x)
  (if (bound? x) x
      (let ((v ($litbind x)))
	(cond ((not (numberp v))
	       (%warn '|unbound symbol occurs after ^| x))
	      ((or (< v 1.) (> v 127.))
	       (%warn '|index out of bounds after ^| x)))))) 

(defun note-variable (var)
  (push var *rhs-bound-vars*))

(defun bound? (var)
  (or (member var *rhs-bound-vars*)
      (var-dope var)))

(defun note-ce-variable (ce-var)
  (push ce-var *rhs-bound-ce-vars*))

(defun ce-bound? (ce-var)
  (or (member ce-var *rhs-bound-ce-vars*)
      (ce-var-dope ce-var)))

;;; *EOF*
