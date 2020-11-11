;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2019 Symbolics Pte. Ltd. All rights reserved.

;;; This definition was copied from openlibm. Leaving here so we can
;;; compare the availability of functions across implementations.

(defpackage #:cephes
  (:use #:cl)

  ;;; Functions not in C90, C99, nor openlibm
  (:export #:igam			; Lower incomplete gamma, normalised
	   #:igamc			; Upper incomplete gamma, normalised
	   #:gamma			; Gamma function
	   #:lgam			; Log of gamma function
	   #:beta			; Beta function
	   #:lbeta			; Log of beta function
	   #:incbet			; Lower incomplete beta, normalised
	   #:incbi			; Inverse of incomplete beta integral
	   #:ntdr			; Area under the Gaussian probability density function
	   #:ntdri			; Returns x such that the area under the Gaussian PDF is equal to y.
	   #:erf			; Error function
	   #:erfc			; Complementary error function
	   #:erfinv			; Inverse of the error function
	   #:erfinvc))			; Inverse of the complementary error function



