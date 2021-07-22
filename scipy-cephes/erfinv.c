/*
 * mconf configures NANS, INFINITYs etc. for cephes and includes some standard
 * headers. Although erfinv and erfcinv are not defined in cephes, erf and erfc
 * are. We want to keep the behaviour consistent for the inverse functions and
 * so need to include mconf.
 */
#include "mconf.h"

/*
 * Inverse of the error function.
 *
 * Computes the inverse of the error function on the restricted domain
 * -1 < y < 1. This restriction ensures the existence of a unique result
 * such that erf(erfinv(y)) = y.
 */
double erfinv(double y) {
  const double domain_lb = -1;
  const double domain_ub = 1;

  if ((domain_lb < y) && (y < domain_ub)) {
    return ndtri(0.5 * (y+1)) * M_SQRT1_2;
  }
  else if (y == domain_lb) {
    return -INFINITY;
  }
  else if (y == domain_ub) {
    return INFINITY;
  }
  else if (cephes_isnan(y)) {
    mtherr( "erfinv", DOMAIN );
    return y;
  }
  else {
    mtherr( "erfinv", DOMAIN );
    return NAN;
  }
}

/*
 * Inverse of the complementary error function.
 *
 * Computes the inverse of the complimentary error function on the restricted
 * domain 0 < y < 2. This restriction ensures the existence of a unique result
 * such that erfc(erfcinv(y)) = y.
 */
double erfcinv(double y) {
  const double domain_lb = 0;
  const double domain_ub = 2;

  if ((domain_lb < y) && (y < domain_ub)) {
    return -ndtri(0.5 * y) * M_SQRT1_2;
  }
  else if (y == domain_lb) {
    return INFINITY;
  }
  else if (y == domain_ub) {
    return -INFINITY;
  }
  else if (cephes_isnan(y)) {
    mtherr( "erfcinv", DOMAIN );
    return y;
  }
  else {
    mtherr( "erfcinv", DOMAIN );
    return NAN;
  }
}
