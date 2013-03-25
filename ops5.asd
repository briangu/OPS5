;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(in-package :asdf)

;; #-(or openmcl mcl sbcl cmu scl clisp lispworks ecl allegro cormanlisp abcl)
;; (error "Sorry, this Lisp is not yet supported.  Patches welcome!")

(defsystem :ops5
  :version "2.0.0.0"
  :description "VPS2 -- Interpreter for OPS5"
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
             
