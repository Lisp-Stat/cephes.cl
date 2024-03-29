/*
 * Cephes Math Library Release 2.8: June, 2000
 * Copyright 1984, 1987, 2000 by Stephen L. Moshier
 */

#include "mconf.h"

extern double MACHEP, PI;


/*
 * Bessel function of noninteger order
 */
double yv(double v, double x)
{
  double y, t;
  int n;

  n = v;
  if (n == v) {
    y = yn(n, x);
    return (y);
  }
  else if (v == floor(v)) {
    /* Zero in denominator. */
    mtherr( "yv", DOMAIN );
    return NAN;
  }

  t = PI * v;
  y = (cos(t) * jv(v, x) - jv(-v, x)) / sin(t);

  if (cephes_isinf(y)) {
    if (v > 0) {
      mtherr( "yv", OVERFLOW );
      return -INFINITY;
    }
    else if (v < -1e10) {
      /* Whether it's +inf or -inf is numerically ill-defined. */
      mtherr( "yv", DOMAIN );
      return NAN;
    }
  }

  return (y);
}
