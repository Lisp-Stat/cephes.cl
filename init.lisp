;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CEPHES -*-
;;; Copyright (c) 2019-2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:cephes)

(cffi:define-foreign-library cephes
  (:windows (:or "libmd"
		 #.(merge-pathnames "scipy-cephes/libmd.dll" *compile-file-pathname*)))
  (:darwin (:or "libmd"
		#+x86-64 #.(merge-pathnames "scipy-cephes/libmd-x86-64.dylib" *compile-file-pathname*)
		#+arm64 #.(merge-pathnames "scipy-cephes/libmd-arm64.dylib" *compile-file-pathname*)))
  (:unix (:or "libmd"
              #.(merge-pathnames "scipy-cephes/libmd.so" *compile-file-pathname*)))
  (t (:default "libmd")))

(cffi:load-foreign-library 'cephes)



