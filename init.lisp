;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CEPHES -*-
;;; Copyright (c) 2019-2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:cephes)

(cffi:define-foreign-library cephes
  (:windows (:or "libmd"
             #.(merge-pathnames "scipy-cephes/libmd.dll" *compile-file-pathname*)))
  (t (:default "libmd")))

(cffi:load-foreign-library 'cephes)



