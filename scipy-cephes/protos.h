#ifndef __SCIPY_SPECIAL_CEPHES
#define __SCIPY_SPECIAL_CEPHES

/* Complex numeral. */
/*
typedef struct {
    double r;
    double i;
} cmplx;
*/
extern double acosh(double x);
extern int airy(double x, double *ai, double *aip, double *bi,
		double *bip);
extern double asin(double x);
extern double acos(double x);
extern double asinh(double x);
extern double atan(double x);
extern double atan2(double y, double x);
extern double atanh(double x);
extern double bdtrc(int k, int n, double p);
extern double bdtr(int k, int n, double p);
extern double bdtri(int k, int n, double y);
extern double beta(double a, double b);
extern double lbeta(double a, double b);
extern double btdtr(double a, double b, double x);
extern double cbrt(double x);
extern double chbevl(double x, double P[], int n);
extern double chdtrc(double df, double x);
extern double chdtr(double df, double x);
extern double chdtri(double df, double y);

/*
 * extern void clog ( cmplx *z, cmplx *w );
 * extern void cexp ( cmplx *z, cmplx *w );
 * extern void csin ( cmplx *z, cmplx *w );
 * extern void ccos ( cmplx *z, cmplx *w );
 * extern void ctan ( cmplx *z, cmplx *w );
 * extern void ccot ( cmplx *z, cmplx *w );
 * extern void casin ( cmplx *z, cmplx *w );
 * extern void cacos ( cmplx *z, cmplx *w );
 * extern void catan ( cmplx *z, cmplx *w );
 * extern void cadd ( cmplx *a, cmplx *b, cmplx *c );
 * extern void csub ( cmplx *a, cmplx *b, cmplx *c );
 * extern void cmul ( cmplx *a, cmplx *b, cmplx *c );
 * extern void cdiv ( cmplx *a, cmplx *b, cmplx *c );
 * extern void cmov ( void *a, void *b );
 * extern void cneg ( cmplx *a );
 */
/*extern double cabs ( cmplx *z ); */
/* extern void csqrt ( cmplx *z, cmplx *w ); */
extern double cosh(double x);
extern double dawsn(double xx);
extern void eigens(double A[], double RR[], double E[], int N);
extern double ellie(double phi, double m);
extern double ellik(double phi, double m);
extern double ellpe(double x);
extern int ellpj(double u, double m, double *sn, double *cn, double *dn,
		 double *ph);
extern double ellpk(double x);
extern double exp(double x);
extern double exp10(double x);
extern double exp1m(double x);
extern double exp2(double x);
extern double expn(int n, double x);
extern double fac(int i);
extern double fdtrc(double a, double b, double x);
extern double fdtr(double a, double b, double x);
extern double fdtri(double a, double b, double y);

/*
 * extern int fftr ( double x[], int m0, double sine[] );
 */
extern int fresnl(double xxa, double *ssa, double *cca);
extern double Gamma(double x);
extern double lgam(double x);
extern double lgam_sgn(double x, int *sign);
extern double lgam1p(double x);
extern double gdtr(double a, double b, double x);
extern double gdtrc(double a, double b, double x);
extern int gels(double A[], double R[], int M, double EPS, double AUX[]);
extern double hyp2f1(double a, double b, double c, double x);
extern double hyperg(double a, double b, double x);
extern double hyp2f0(double a, double b, double x, int type, double *err);
extern double i0(double x);
extern double i0e(double x);
extern double i1(double x);
extern double i1e(double x);
extern double igamc(double a, double x);
extern double igam(double a, double x);
extern double igam_fac(double a, double x);
extern double igamci(double a, double q);
extern double igami(double a, double p);
extern double incbet(double aa, double bb, double xx);
extern double incbi(double aa, double bb, double yy0);
extern double iv(double v, double x);
extern double j0(double x);
extern double y0(double x);
extern double j1(double x);
extern double y1(double x);
extern double jn(int n, double x);
extern double jv(double n, double x);
extern double k0(double x);
extern double k0e(double x);
extern double k1(double x);
extern double k1e(double x);
extern double kn(int nn, double x);

/*
 * extern int levnsn ( int n, double r[], double a[], double e[], double refl[] );
 */
extern double log(double x);
extern double log10(double x);

/*
 * extern double log2 ( double x );
 */
extern long lrand(void);
extern long lsqrt(long x);
/* extern void mtherr(const char *name, int code); */
extern double nbdtrc(int k, int n, double p);
extern double nbdtr(int k, int n, double p);
extern double nbdtri(int k, int n, double p);
extern double ndtr(double a);
extern double erfc(double a);
extern double erf(double x);
extern double ndtri(double y0);
extern double pdtrc(int k, double m);
extern double pdtr(int k, double m);
extern double pdtri(int k, double y);
extern double psi(double x);
extern void revers(double y[], double x[], int n);
extern double rgamma(double x);
extern double round(double x);
extern int shichi(double x, double *si, double *ci);
extern int sici(double x, double *si, double *ci);
extern double sin(double x);
extern double cos(double x);
extern double radian(double d, double m, double s);

extern double sindg(double x);
extern double cosdg(double x);
extern double sinh(double x);
extern double spence(double x);
extern double sqrt(double x);
extern double stdtr(int k, double t);
extern double stdtri(int k, double p);
extern double onef2(double a, double b, double c, double x, double *err);
extern double threef0(double a, double b, double c, double x, double *err);
extern double struve(double v, double x);
extern double tan(double x);
extern double cot(double x);
extern double tandg(double x);
extern double cotdg(double x);
extern double tanh(double x);
extern double log1p(double x);
extern double log1pmx(double x);
extern double expm1(double x);
extern double cosm1(double x);
extern double yn(int n, double x);
extern double zeta(double x, double q);
extern double zetac(double x);
extern int drand(double *a);

double yv(double v, double x);

extern double lanczos_sum(double x);
extern double lanczos_sum_expg_scaled(double x);
extern double lanczos_sum_near_1(double dx);
extern double lanczos_sum_near_2(double dx);

#endif
