;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2021-2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

;; Pull requests to fix ACL make-path welcome.
;; See: https://franz.com/support/documentation/10.1/doc/pathnames.htm#parsing-windows-pathnames-1
;; The problem we see here is describe in that section of the
;; documentation.  It needs to be fixed so that it works on all
;; supported implementations: ACL, CCL and SBCL.

#+allegro (format *error-output* "Not attempting automatic build on ACL due to non-standard make-pathname.  Please compile libmd manually if it doesn't load on your ACL system.")

#-allegro
;; Define a makefile as a kind of source file in ASDF
(progn
  (defclass makefile (source-file) ((type :initform "m")))
  (defmethod perform ((o load-op) (c makefile)) t)
  (defmethod perform ((o compile-op) (c makefile))
    (let* ((lib-dir (system-relative-pathname "cephes" "scipy-cephes/"))
           (lib (make-pathname :directory (pathname-directory lib-dir)
                               :name #+(or (and unix (not darwin)) windows win32) "libmd"
			       #+(and darwin arm64) "libmd-arm64"
			       #+(and darwin x86-64) "libmd-x86-64"
			       :device (pathname-device lib-dir)
                               :type #+darwin "dylib"
				     #+(and unix (not darwin)) "so"
				     #+(or windows win32) "dll"))
	   (built (probe-file (namestring lib))))
      (if built
	  (format *error-output* "Library ~S exists, skipping build" lib)
	  (format *error-output* "Building ~S~%" lib))
      (unless built
	(chdir (native-namestring lib-dir))
	(run-program "make" :output t)))))

(defsystem "cephes"
  :description "Wrapper for the Cephes Mathematical Library"
  :version     "1.4.2"
  :author      "Steven Nunez <steve@symbolics.tech>"
  :license     :MS-PL
  :depends-on ("cffi")
  :serial t
  :components (#-allegro (:module "libmd"
			  :components ((:makefile "makefile")))
	       (:file "package")
	       (:file "init")
               (:file "cephes")))
