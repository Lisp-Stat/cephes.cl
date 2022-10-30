;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CEPHES -*-
;;; Copyright (c) 2020,2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:cephes)

;;; Listed in alphabetical order

;; airy.c
(cffi:defcfun ("airy" cephes-airy) :int	;Airy function
  "Solution of the differential equation y''(x) = xy
The function returns the two independent solutions Ai, Bi and their first derivatives Ai'(x), Bi'(x)."
  (x :double) (ai (:pointer :double)) (aip (:pointer :double)) (bi (:pointer :double)) (bip (:pointer :double)))

(defun airy (x)
    "Solution of the differential equation y''(x) = xy
The function returns the two independent solutions Ai, Bi and their first derivatives Ai'(x), Bi'(x), as VALUES (Ai Bi Aip Bip)"
  (cffi:with-foreign-objects ((ai :double)(aip :double)(bi :double)(bip :double))
    (cephes-airy x ai aip bi bip)
    (values (cffi:mem-ref ai :double)
	    (cffi:mem-ref bi :double)
	    (cffi:mem-ref aip :double)
	    (cffi:mem-ref bip :double))))

;;; Binomial

;; bdtr.c
(cffi:defcfun "bdtr" :double		;Binomial distribution
  "Returns the sum of the terms 0 through k of the Binomial probability density"
  (k :double)(n :int)(p :double))
(cffi:defcfun "bdtrc" :double	        ;Complement of binomial distribution
  "Returns the sum of the terms k+1 through n of the Binomial probability density"
    (k :double)(n :int)(p :double))
(cffi:defcfun "bdtri" :double		;Inverse binomial distribution
  "Finds the event probability p such that the sum of the terms 0 through k of the Binomial probability density is equal to the given cumulative probability y."
    (k :int)(n :int)(y :double))


;;; besselpoly.c
(cffi:defcfun "besselpoly" :double	;Weighted integral of the Bessel function of the first kind
  (a :double)(lambda :double)(nu :double))


;;; Beta

;; beta.c
(cffi:defcfun "beta"  :double (a :double)(b :double)) ;Beta function
(cffi:defcfun "lbeta" :double (a :double)(b :double)) ;Natural log of |beta|

;; btdtr.c
(cffi:defcfun "btdtr" :double			  ;incomplete beta integral
  "Returns the area from zero to x under the beta density function.

                          x
            -             -
           | (a+b)       | |  a-1      b-1
 P(x)  =  ----------     |   t    (1-t)    dt
           -     -     | |
          | (a) | (b)   -
                         0

This function is identical to the incomplete beta integral function incbet(a, b, x)."
  (a :double)(b :double)(x :double))



;; cbrt.c
(cffi:defcfun "cbrt" :double		;cube root
  "Returns the cube root of the argument, which may be negative."
  (x :double))

#| TODO - Need to wrap the CL array
Also note that we have this in numerical-utilities
(cffi:defcfun "chbevl" :double		;evaluate Chebyshev series
  "Evaluates the series
       N-1
        - '
 y  =   >   coef[i] T (x/2)
        -            i
       i=0

of Chebyshev polynomials Ti at argument x/2.

Coefficients are stored in reverse order, i.e. the zero
order term is last in the array.  Note N is the number of
coefficients, not the order.

If coefficients are for the interval a to b, x must
have been transformed to x -> 2(2x - b - a)/(b-a) before
entering the routine.  This maps x from (a, b) to (-1, 1),
over which the Chebyshev polynomials are defined.

If the coefficients are for the inverted interval, in
which (a, b) is mapped to (1/b, 1/a), the transformation
required is x -> 2(2ab/x - b - a)/(b-a).  If b is infinity,
this becomes x -> 4a/x - 1."
  (x :double)(

;; extern double chbevl(double x, double array[], int n);
|#

;;; Chi-square

;; chdtr.c
(cffi:defcfun "chdtr" :double		  ;Chi-square distribution
  "Returns the area under the left hand tail (from 0 to x) of the Chi square probability density function with DF degrees of freedom."
  (df :double)(x :double))
(cffi:defcfun "chdtrc" :double		;Complemented Chi-square distribution
  "Returns the area under the right hand tail (from x to infinity) of the Chi square probability density function with DF degrees of freedom"
  (df :double)(x :double))
(cffi:defcfun "chdtri" :double		;Inverse of complemented Chi-square distribution
  "Finds the Chi-square argument x such that the integral from x to infinity of the Chi-square density is equal to the given cumulative probability y"
  (df :double)(y :double))


;; dawsn.c
(cffi:defcfun "dawsn" :double		;Dawson's Integral
  (xx :double))


;;; Elliptic

;; ellik.c
(cffi:defcfun "ellik" :double		  ;Incomplete elliptic integral of the first kind
  (phi :double)(m :double))

;; ellie.c
(cffi:defcfun "ellie" :double		  ;Incomplete elliptic integral of the second kind
  (phi :double)(m :double))

;; ellpk.c
(cffi:defcfun "ellpk" :double		;Complete elliptic integral of the first kind
  (x :double))

;; ellpe.c
(cffi:defcfun "ellpe" :double		;Complete elliptic integral of the second kind
  (x :double))

;; ellpj.c
(cffi:defcfun ("ellpj" cephes-ellpj) :double		;Jacobian Elliptic Functions
  (u :double)(m :double)
  (sn (:pointer :double))(cn (:pointer :double))(dn (:pointer :double))(phi (:pointer :double)))

(defun jacobian-elliptic (u m)
  "Evaluates the Jacobian elliptic functions sn(u|m), cn(u|m), and dn(u|m) of parameter m between 0 and 1, and real argument u. Returns VALUES (sn cn dn)"
  (cffi:with-foreign-objects ((sn :double)(cn :double)(dn :double)(phi :double))
    (cephes-ellpj u m sn cn dn phi)
    (values (cffi:mem-ref sn :double)
	    (cffi:mem-ref cn :double)
	    (cffi:mem-ref dn :double))))


;;; Exponential functions

;; exp2.c
(cffi:defcfun "exp2" :double		;Base 2 exponential function
  "Returns 2 raised to the x power."
  (x :double))

;; exp10.c
(cffi:defcfun "exp10" :double		;Base 10 exponential function (Common antilogarithm)
  "Returns 10 raised to the x power."
  (x :double))

;; exp1m defined below

;; expn.c
(cffi:defcfun "expn" :double		;Exponential integral
  "Evaluates the exponential integral"
  (n :int)(x :double))


;;; F Distribution

;; fdtr.c
(cffi:defcfun "fdtr" :double		;F distribution
  "Returns the area from zero to x under the F density function"
  (a :double)(b :double)(x :double))
(cffi:defcfun "fdtrc" :double		;Complemented F distribution
  "Returns the area from x to infinity under the F density function"
  (a :double)(b :double)(x :double))
(cffi:defcfun "fdtri" :double		;Inverse of F distribution
  "Finds the F density argument x such that the integral from -infinity to x of the F density is equal to the given probability p"
  (a :double)(b :double)(y :double))


;; fresnl.c
(cffi:defcfun "cephes-fresnl" :int				;Fresnel integral
  "Evaluates S and C fresnel integrals and returns VALUES (S C)"
  (xxa :double)(ssa (:pointer :double))(cca (:pointer :double)))
(defun fresnl (xxa)
  (cffi:with-foreign-objects ((ssa :double)(cca :double))
    (cephes-fresnl xxa ssa cca)
    (values (cffi:mem-ref ssa :double)
	    (cffi:mem-ref cca :double))))


;;; Gamma

;; gamma.c
(cffi:defcfun ("Gamma" gamma) :double	;Gamma function
  "Returns Gamma function of the argument.  The result is correctly signed."
  (x :double))
(cffi:defcfun ("lgam" log-gamma)  :double ;Natural logarithm of Gamma function
  "Returns the base e logarithm of the absolute value of the Gamma function of the argument."
  (x :double))

;; gammasgn.c
(cffi:defcfun ("gammasgn" sign-gamma) :double (x :double)) ;Do we really need to expose this?

;; gdtr.c
(cffi:defcfun "gdtr" :double			  ;Gamma distribution function
  "Returns the integral from zero to x of the Gamma probability density function"
  (a :double)(b :double)(x :double))
(cffi:defcfun "gdtrc" :double			  ;Complemented Gamma distribution function
  "Returns the integral from x to infinity of the Gamma probability density function"
  (a :double)(b :double)(x :double))
(cffi:defcfun "gdtri" :double			  ;Inverse Gamma distribution function (?) - not documented in src
  (a :double)(b :double)(y :double))


;;; Gauss hypergeometric

;; hyp2f1.c
(cffi:defcfun "hyp2f1" :double				     ;Gauss hypergeometric function
  (a :double)(b :double)(c :double)(x :double))

;; hyperg.c
(cffi:defcfun "hyperg" :double			   ;Confluent hypergeometric function
  "Computes the confluent hypergeometric function"
  (a :double)(b :double)(x :double))

;; extern double threef0(double a, double b, double c, double x, double *err);


;;; Bessel

;; i0.c
(cffi:defcfun "i0" :double		;Modified Bessel function of order zero
  "Returns modified Bessel function of order zero of the argument"
  (x :double))
(cffi:defcfun "i0e" :double		;Modified Bessel function of order zero, exponentially scaled
  "Returns exponentially scaled modified Bessel function of order zero of the argument"
  (x :double))

;; i1.c
(cffi:defcfun "i1" :double		;Modified Bessel function of order one
  "Returns modified Bessel function of order one of the argument"
  (x :double))
(cffi:defcfun "i1e" :double		;Modified Bessel function of order one, exponentially scaled
  "Returns exponentially scaled modified Bessel function of order one of the argument"
  (x :double))


;;; Incomplete gamma
;; igam.c
(cffi:defcfun "igam"  :double (a :double) (x :double)) ;Regularized lower incomplete gamma function
(cffi:defcfun "igamc" :double (a :double) (x :double)) ;Regularized upper incomplete gamma function

;; igami.c - inverse incomplete gamma functions
(cffi:defcfun "igami" :double		;Inverse of the lower incomplete gamma function
  "Returns the x such that: igamc(a, x) = p
The input argument a must be positive and p must be between 0 and 1."
  (a :double) (p :double))
(cffi:defcfun "igamci" :double (a :double) (q :double)) ;Inverse of the upper incomplete gamma function


;;; Incomplete beta
;; incbet.c
(cffi:defcfun "incbet" :double		;Incomplete beta integral
  "Returns incomplete beta integral of the arguments, evaluated from zero to x."
  (aa :double)(bb :double)(xx :double))

;; incbi.c
(cffi:defcfun "incbi" :double		;Inverse of incomplete beta integral
  "Given y, the function finds x such that incbet( a, b, x ) = y"
  (aa :double)(bb :double)(yy0 :double))


;;; More Bessel functions

;; scipy_iv.c
(cffi:defcfun "iv" :double		; Modified Bessel function of noninteger order
  "Returns modified Bessel function of order v of the argument.  If x is negative, v must be integer valued."
  (v :double)(x :double))

;; j0.c
(cffi:defcfun ("J0" j0) :double		;Bessel function of order zero
  "Returns Bessel function of order zero of the argument"
  (x :double))
(cffi:defcfun ("Y0" y0) :double		;Bessel function of the second kind, order zero
  "Bessel function of the second kind, order zero"
  (x :double))

;; j1.c
(cffi:defcfun ("J1" j1) :double		;Bessel function of order one
  "Returns Bessel function of order one of the argument."
  (x :double))
(cffi:defcfun ("Y1" y1) :double		;Bessel function of second kind of order one
  "Returns Bessel function of the second kind of order one of the argument."
  (x :double))

;; jv.c
(cffi:defcfun "jv" :double		;Bessel function of noninteger order
  "Returns Bessel function of order v of the argument, where v is real.  Negative x is allowed if v is an integer."
  (v :double)(x :double))

;; extern double jn(int n, double x) ; This function does not exist

;; k0.c
(cffi:defcfun "k0" :double		;Modified Bessel function, third kind, order zero
  "Returns modified Bessel function of the third kind of order zero of the argument."
  (x :double))
(cffi:defcfun "k0e" :double		;Modified Bessel function, third kind, order zero, exponentially scaled
  "Returns exponentially scaled modified Bessel function of the third kind of order zero of the argument."
  (x :double))

;; k1.c
(cffi:defcfun "k1" :double
  "Computes the modified Bessel function of the third kind of order one of the argument."
  (x :double))
(cffi:defcfun "k1e" :double
  "Returns exponentially scaled modified Bessel function of the third kind of order one of the argument"
  (x :double))

;; kn.c
(cffi:defcfun "kn" :double		;Modified Bessel function, third kind, integer order
  "Returns modified Bessel function of the third kind of order n of the argument"
  (nn :int)(x :double))


;;; Negative binomial

;; nbdtr.c
(cffi:defcfun "nbdtr" :double		     ;Negative binomial distribution
  "Returns the sum of the terms 0 through k of the negative binomial distribution"
  (k :int)(n :int)(p :double))
(cffi:defcfun "nbdtrc" :double		     ;Complemented negative binomial distribution
  "Returns the sum of the terms k+1 to infinity of the negative binomial distribution"
  (k :int)(n :int)(p :double))
(cffi:defcfun "nbdtri" :double		     ;Complemented negative binomial distribution
  "Returns the sum of the terms k+1 to infinity of the negative binomial distribution"
    (k :int)(n :int)(p :double))


;;; Normal

;; ndtr.c
(cffi:defcfun "ndtr" :double		;Normal distribution function
  "Returns the area under the Gaussian probability density function, integrated from minus infinity to x"
  (a :double))
(cffi:defcfun ("log_ndtr" log-ndtr) :double (a :double)) ;Log of the normal distribution function
(cffi:defcfun "erf"  :double (x :double)) ;Error function
(cffi:defcfun "erfc" :double (a :double)) ;Complementary error function

;; erfinv.c
(cffi:defcfun "erfinv" :double
  "Inverse of the error function.
 Computes the inverse of the error function on the restricted domain
 -1 < y < 1. This restriction ensures the existence of a unique result
 such that erf(erfinv(y)) = y."
  (y :double))
(cffi:defcfun "erfcinv" :double		;Inverse of the complementary error function
  "Computes the inverse of the complimentary error function on the restricted domain 0 < y < 2.
This restriction ensures the existence of a unique result such that erfc(erfcinv(y)) = y."
  (y :double))

;; ndtri.c
(cffi:defcfun "ndtri" :double		;Inverse of Normal distribution function
  "Returns the argument, x, for which the area under the Gaussian probability density function (integrated from minus infinity to x) is equal to y.
For small arguments 0 < y < exp(-2), the program computes z = sqrt( -2.0 * log(y) ); then the approximation is x = z - log(z)/z  - (1/z) P(1/z) / Q(1/z)."
  (y0 :double))


;;; Poisson distribution

;; pdtr.c
(cffi:defcfun "pdtr" :double		;Poisson distribution
  "Returns the sum of the first k terms of the Poisson distribution"
  (k :double)(m :double))
(cffi:defcfun "pdtrc" :double		;Complemented poisson distribution
  "Returns the sum of the terms k+1 to infinity of the Poisson distribution"
    (k :double)(m :double))
(cffi:defcfun "pdtri" :double		;Inverse Poisson distribution
  "Finds the Poisson variable x such that the integral from 0 to x of the Poisson density is equal to the given probability y"
  (k :int)(y :double))



;; poch.c
(cffi:defcfun "poch" :double		;Pochhammer symbol (a)_m = gamma(a + m) / gamma(a)
  (x :double)(m :double))

;; psi.c
(cffi:defcfun "psi" :double		;Psi (digamma) function
  "Returns the logarithmic derivative of the gamma function"
  (x :double))

;; rgamma.c
(cffi:defcfun "rgamma" :double		;Reciprocal Gamma function
  "Returns one divided by the Gamma function of the argument"
  (x :double))

;; round.c
;; This interferes with CL:ROUND
;; (cffi:defcfun "round" :double		;Round double to nearest or even integer valued double
;;   "Returns the nearest integer to x as a double precision floating point result.  If x ends in 0.5 exactly, the nearest even integer is chosen"
;;   (x :double))

;; shichi.c
(cffi:defcfun ("shichi" cephes-shichi) :double			    ;Hyperbolic sine and cosine integrals
  (x :double)(si (:pointer :double))(ci (:pointer :double)))
(defun shichi (x)
  "Returns VALUES (si ci)"
  (cffi:with-foreign-objects ((si :double)(ci :double))
    (cephes-shichi x si ci)
    (values (cffi:mem-ref si :double)
	    (cffi:mem-ref ci :double))))

;; sici.c
(cffi:defcfun ("sici" cephes-sici) :double ;Sine and cosine integrals
  (x :double)(si (:pointer :double))(ci (:pointer :double)))
(defun sici (x)
  "Returns VALUES (si ci)"
  (cffi:with-foreign-objects ((si :double)(ci :double))
    (cephes-sici x si ci)
    (values (cffi:mem-ref si :double)
	    (cffi:mem-ref ci :double))))

;; sindg.c
(cffi:defcfun "sindg" :double			   ;Circular sine of angle in degrees
  "Range reduction is into intervals of 45 degrees."
  (d :double)(m :double)(s :double))
(cffi:defcfun "cosdg" :double			   ;Circular cosine of angle in degrees
  "Range reduction is into intervals of 45 degrees."
  (d :double)(m :double)(s :double))


;; sinpi.c
;; Since the periods of these functions are integral (and thus
;; representable in double precision), it's possible to compute them
;; with greater accuracy than sin(x) and cos(x)
(cffi:defcfun "sinpi" :double		;Compute sin(pi * x)
  (x :double))
(cffi:defcfun "cospi" :double		;Compute cos(pi * x)
  (x :double))


;; spence.c
(cffi:defcfun "spence" :double		;Dilogarithm
  (x :double))


;;; Student's T distribution

;; stdtr.c
(cffi:defcfun "stdtr" :double		;Student's t distribution
  "Computes the integral from minus infinity to t of the Student t distribution with integer k > 0 degrees of freedom"
  (k :int)(t1 :double))
(cffi:defcfun "stdtri" :double		;Functional inverse of Student's t distribution
  "Given probability p, finds the argument t such that stdtr(k,t) is equal to p"
  (k :int)(p :double))


;; yv.c
(cffi:defcfun "yv" :double		;Bessel function of noninteger order
  (v :double)(x :double))


;; tandg.c
(cffi:defcfun "tandg" :double
  "Returns the circular tangent of the argument x in degrees"
  (x :double))
(cffi:defcfun "cotdg" :double
  "Returns the circular cotangent of the argument x in degrees"
  (x :double))


;; unity.c
(cffi:defcfun "log1p"   :double (x :double)) ;log(1 + x)
(cffi:defcfun "log1pmx" :double (x :double)) ;log(1 + x) - x
(cffi:defcfun "expm1"   :double (x :double)) ;exp(x) - 1
(cffi:defcfun "cosm1"   :double (x :double)) ;cos(x) - 1
(cffi:defcfun "lgam1p"  :double (x :double)) ;lgam(x + 1)


;; yn.c
(cffi:defcfun "yn" :double		;Bessel function of second kind of integer order
  " Returns Bessel function of order n, where n is a (possibly negative) integer"
  (n :int)(x :double))

;; zeta.c
(cffi:defcfun "zeta" :double		;Riemann zeta function of two arguments
  (x :double)(q :double))


;; zetac.c
(cffi:defcfun "zetac" :double		;Riemann zeta function
  (x :double))


;; lanczos.c
(cffi:defcfun ("lanczos_sum" lanczos-sum) :double (x :double))
(cffi:defcfun ("lanczos_sum_expg_scaled" lanczos-sum-scaled) :double (x :double)) ;note g=6.024680040776729583740234375, not 13... as is common
(cffi:defcfun ("lanczos_sum_near_1" lanczos-sum-near-1) :double (x :double))
(cffi:defcfun ("lanczos_sum_near_2" lanczos-sum-near-2) :double (x :double))

;; owens_t.c
(cffi:defcfun ("owens_t" owens-t) :double (h :double) (a :double)) ;Owen's T-Function


;; poly.h
(cffi:defcfun "evlrat" :double
  "Evaluate a rational function"
  (x :double)(num (:pointer :double))(m :int)(denom (:pointer :double))(n :int))
