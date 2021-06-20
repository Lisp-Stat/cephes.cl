;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CEPHES -*-
;;; Copyright (c) 2020,2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:cephes)


;;; Gamma

;; igam.c
(cffi:defcfun "igam"  :double (a :double)(y :double)) ;Incomplete Gamma integral
(cffi:defcfun "igamc" :double (a :double)(y :double)) ;Complemented incomplete Gamma integral

;; gamma.c
(cffi:defcfun "lgam"  :double (x :double))  ;Returns the base e logarithm of the absolute value of the Gamma function of the argument.
(cffi:defcfun "gamma" :double (x :double)) ; Returns Gamma function of the argument.  The result is correctly signed.


;;; Beta

;; beta.c
(cffi:defcfun "beta"  :double (a :double)(b :double)) ;Beta function
(cffi:defcfun "lbeta" :double (a :double)(b :double)) ; log of the beta function

;; incbet.c
(cffi:defcfun "incbet" :double (a :double)(b :double)(x :double)) ;Returns incomplete beta integral of the arguments, evaluated from zero to x.

;; incbi.c
(cffi:defcfun "incbi" :double (a :double)(b :double)(x :double))  ;Given y, the function finds x such that incbet( a, b, x ) = y .


;;; Normal

;; ndtr.c
(cffi:defcfun "ndtr" :double (x :double)) ;Returns the area under the Gaussian probability density function, integrated from minus infinity to x
(cffi:defcfun "erf"  :double (x :double))
(cffi:defcfun "erfc" :double (x :double))

;; ndtri.c
(cffi:defcfun "ndtri" :double (y :double)) ;Inverse of the error function

(defun erfinv (y)			; This is the same calculation Python uses. Not accurate when y is much less than 1
  "Inverse of the error function"	; See https://github.com/scipy/scipy/issues/12758
  (* (cephes:ndtri (* 0.5d0
		      (1+ y)))
     (/ 1 (sqrt 2))))


;;; Other

;; unity.c
(cffi:defcfun "log1p" :double (x :double)) ;log(1 + x)
(cffi:defcfun "expm1" :double (x :double)) ;exp(x) - 1
(cffi:defcfun "cosm1" :double (x :double)) ;cos(x) - 1


#| These are left to do
airy.c          chbevl.c    dd_real_idefs.h  exp10.c   gammasgn.c            k0.c          nbdtr.c                sindg.c   unity.c
bdtr.c          chdtr.c     ellie.c          exp2.c    gdtr.c      igami.c   k1.c                     psi.c       sinpi.c
besselpoly.c    const.c     ellik.c          expn.c    hyp2f1.c              kn.c                     rgamma.c    spence.c  yn.c
                dawsn.c     ellpe.c          expn.h    hyperg.c              kolmogorov.c  owens_t.c  round.c     stdtr.c   yv.c
btdtr.c         dd_idefs.h  ellpj.c          fdtr.c    i0.c                  lanczos.c     pdtr.c                 struve.c  zeta.c
                dd_real.c   ellpk.c          fresnl.c  i1.c                  lanczos.h     poch.c     shichi.c    tandg.c   zetac.c
                dd_real.h                                                                  polevl.h   sici.c      tukey.c
|#
