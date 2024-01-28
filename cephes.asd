;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2021-2024 by Symbolics Pte. Ltd. All rights reserved.

;; Define a makefile as a kind of source file in ASDF
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
      (run-program "make" :output t))))

(defsystem "cephes"
  :description "Wrapper for the Cephes Mathematical Library"
  :version     "1.4.1"
  :author      "Steven Nunez <steve@symbolics.tech>"
  :license     :MS-PL
  :depends-on ("cffi")
  :serial t
  :components ((:module "libmd"
	        :components ((:makefile "makefile")))
	       (:file "package")
	       (:file "init")
               (:file "cephes")))
