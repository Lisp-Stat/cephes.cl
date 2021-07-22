# CEPHES.CL
  A common lisp CFFI wrapper for the [Scipy version of Cephes special functions](https://github.com/scipy/scipy/tree/master/scipy/special/cephes).

For many years Cephes was considered the gold standard in
cross-platform mathematical libraries, superior to the libm
distributions of the time (early 1990's).  Since then C99 and later
ANSI C specifications have closed the gap, but there are still certain
special and statistical functions available in Cephes that are not in
the standard C libraries.

The Scipy version differs from standard Cephes in that it has some
additional functions, improvements in accuracy, and is better
documented.  Only double-float versions are provided.

# Installation

You will need to build the C shared library manually according to the
conventions of your system.  As delivered, the Makefile is set-up to
work on MS Windows, and you can build it like so:

```sh
cd scipy-cephes
make libmd.dll
```

If you are running on UNIX, you'll need to add a target that builds a
shared library on that platform.  If you do this, please drop a note
into a Cephes [repository
issue](https://github.com/Lisp-Stat/cephes.cl/issues) with the build
instructions so we can update the Makefile.

The `init.lisp` file, where CFFI loads the library, should work out of
the box if `libmd` on the path somewhere, regardless of platform.

# Documentation

If you know your way around special functions, the table below should
suffice to get started.  For a more complete description, see the doc
strings in cephes.lisp.  Finally, the C source code itself, referenced
in =cephes.lisp=, is through and complete.

You can also use the
[scipy.special](https://docs.scipy.org/doc/scipy/reference/special.html#module-scipy.special)
online documentation.

# The API
  There is no overlap between the wrapped Cephes functions and the
  Common Lisp numerical tower. All functions are in the =cephes=
  package.

  Currently exported functions are:

| function          | description                                                                   |
|-------------------|-------------------------------------------------------------------------------|
| airy              | Airy function                                                                 |
| bdtr              | Binomial distribution                                                         |
| bdtrc             | Complement of binomial distribution                                           |
| bdtri             | Inverse binomial distribution                                                 |
| besselpoly        | Weighted integral of the Bessel function of the first kind                    |
| beta              | Beta function                                                                 |
| lbeta             | Natural log of beta                                                           |
| btdtr             | incomplete beta integral                                                      |
| cbrt              | cube root                                                                     |
| chdtr             | Chi-square distribution                                                       |
| chdtrc            | Complemented Chi-square distribution                                          |
| chdtri            | Inverse of complemented Chi-square distribution                               |
| dawsn             | Dawson's Integral                                                             |
| ellik             | Incomplete elliptic integral of the first kind                                |
| ellie             | Incomplete elliptic integral of the second kind                               |
| ellpk             | Complete elliptic integral of the first kind                                  |
| ellpe             | Complete elliptic integral of the second kind                                 |
| jacobian-elliptic | jacobian Elliptic Functions                                                   |
| exp2              | Base 2 exponential function                                                   |
| exp10             | Base 10 exponential function (Common antilogarithm)                           |
| expn              | Exponential integral                                                          |
| fdtr              | F distribution                                                                |
| fdtrc             | Complemented F distribution                                                   |
| fdtri             | Inverse of F distribution                                                     |
| fresnl            | Fresnel integral                                                              |
| gamma             | Gamma function                                                                |
| log-gamma         | Natural logarithm of Gamma function                                           |
| gdtr              | Gamma distribution function                                                   |
| gdtrc             | Complemented Gamma distribution function                                      |
| gdtri             | Inverse Gamma distribution function (?) - not documented in src               |
| hyp2f1            | Gauss hypergeometric function                                                 |
| hyperg            | Confluent hypergeometric function                                             |
| i0                | Modified Bessel function of order zero                                        |
| i0e               | Modified Bessel function of order zero, exponentially scaled                  |
| i1                | Modified Bessel function of order one                                         |
| i1e               | Modified Bessel function of order one, exponentially scaled                   |
| igam              | Regularized lower incomplete gamma function                                   |
| igamc             | Regularized upper incomplete gamma function                                   |
| igami             | Inverse of the lower incomplete gamma function                                |
| igamci            | Inverse of the upper incomplete gamma function                                |
| incbet            | Incomplete beta integral                                                      |
| incbi             | Inverse of incomplete beta integral                                           |
| iv                | Modified Bessel function of noninteger order                                  |
| j0                | Bessel function of order zero                                                 |
| y0                | Bessel function of the second kind, order zero                                |
| j1                | Bessel function of order one                                                  |
| y1                | Bessel function of second kind of order one                                   |
| jv                | Bessel function of noninteger order                                           |
| k0                | Modified Bessel function, third kind, order zero                              |
| k0e               | Modified Bessel function, third kind, order zero, exponentially scaled        |
| k1                | Modified Bessel function of the third kind of order one                       |
| k1e               | Modified Bessel function of the third kind of order one, exponentially scaled |
| kn                | Modified Bessel function, third kind, integer order                           |
| nbdtr             | Negative binomial distribution                                                |
| nbdtrc            | Complemented negative binomial distribution                                   |
| nbdtri            | Inverse complemented negative binomial distribution                           |
| ndtr              | Normal distribution function                                                  |
| log-ndtr          | Log of the normal distribution function                                       |
| erf               | Error function                                                                |
| erfc              | Complementary error function                                                  |
| erfinv            | Inverse of the error function                                                 |
| erfcinv           | Inverse of the complementary error function                                   |
| ndtri             | Inverse of Normal distribution function                                       |
| pdtr              | Poisson distribution                                                          |
| pdtrc             | Complemented poisson distribution                                             |
| pdtri             | Inverse Poisson distribution                                                  |
| poch              | Pochhammer symbol (a)_m = gamma(a + m) / gamma(a)                             |
| psi               | Psi (digamma) function                                                        |
| rgamma            | Reciprocal Gamma function                                                     |
| shichi            | Hyperbolic sine and cosine integrals                                          |
| sici              | Sine and cosine integrals                                                     |
| sindg             | Circular sine of angle in degrees                                             |
| cosdg             | Circular cosine of angle in degrees                                           |
| sinpi             | Compute sin(pi * x)                                                           |
| cospi             | Compute cos(pi * x)                                                           |
| spence            | Dilogarithm                                                                   |
| stdtr             | Student's t distribution                                                      |
| stdtri            | Functional inverse of Student's t distribution                                |
| yv                | Bessel function of noninteger order                                           |
| tandg             | Circular tangent of angle in degrees                                          |
| cotdg             | Circular cotangent of argument in degrees                                     |
| log1p             | log(1 + x)                                                                    |
| log1pmx           | log(1 + x) - x                                                                |
| expm1             | exp(x) - 1                                                                    |
| cosm1             | cos(x) - 1                                                                    |
| lgam1p            | lgam(x + 1)                                                                   |
| yn                | Bessel function of second kind of integer order                               |
| zeta              | Riemann zeta function of two arguments                                        |
| zetac             | Riemann zeta function                                                         |
| owens-t           | Owen's T-Function                                                             |

# Contributing
  When contributing to this repository, please first discuss major
  changes to the existing code you wish to make via a github
  issue. Minor changes and major additions are welcome. Please [write
  good commit messages](https://chris.beams.io/posts/git-commit/).

# License
  CEPHES.CL is available under the [Microsoft Public
  License](https://opensource.org/licenses/MS-PL).
