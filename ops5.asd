;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(in-package :asdf)

;; #-(or openmcl mcl sbcl cmu scl clisp lispworks ecl allegro cormanlisp abcl)
;; (error "Sorry, this Lisp is not yet supported.  Patches welcome!")

(defsystem :ops5
  :version "19-OCT-92"
  :description "VPS2 -- Interpreter for OPS5

This Common Lisp version of OPS5 is in the public domain.  It is based
in part on based on a Franz Lisp implementation done by Charles L. Forgy
at Carnegie-Mellon University, which was placed in the public domain by
the author in accordance with CMU policies.  Ported to Common Lisp by 
George Wood and Jim Kowalski. CMU Common Lisp modifications by
Dario Guise, Skef Wholey, Michael Parzen, and Dan Kuokka. 
Modified to work in CLtL1, CLtL2 and X3J13 compatible lisps by 
Mark Kantrowitz on 14-OCT-92. The auto.ops and reactor.ops demo files
were provided by Michael Mauldin. In year 2013, ported to modern Common
Lisp implementations and added support for quicklisp by Xiaofeng Yang.

This code is made available is, and without warranty of any kind by the
authors or by Carnegie-Mellon University.

Documentation for OPS may be found in the OPS5 User's Manual, July 1981,
by Forgy, CMU CSD.

This version of OPS5 was obtained by anonymous ftp from 
   ftp.cs.cmu.edu:/user/ai/areas/expert/systems/ops5/ops5_cl.tgz
"
  :author ""
  :maintainer ""
  :licence "public domain"
  :depends-on ()
  :components
  ((:module "src"
            :serial t
            :components
            (
             (:file "ops")			; so that shadowing takes place...
             (:file "ops-globals")
             (:file "ops-util")		; macros
             (:file "ops-compile")
             (:file "ops-rhs")		; defines macros used in ops-init
             (:file "ops-main")		; macros
             (:file "ops-match") 
             (:file "ops-backup")
             (:file "ops-io")
             (:file "ops-init")))))
             
