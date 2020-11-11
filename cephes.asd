;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem #:cephes
  :description "Wrapper for the Cephes Mathematical Library"
  :version      (:read-file-form "version.sexp")
  :author "Steven Nunez <steve@symbolics.tech>"
  :license "MS-PL"
  :depends-on (#:cffi)
  :serial t
  :components ((:file #:package)
	       (:file #:init)
               (:file #:cephes)))
