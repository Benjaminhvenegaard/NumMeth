#include <math.h>

void bessiaplusn(float a, float x, int n, float ia[])
{
	if (x == 0.0) {
		ia[0] = (a == 0.0) ? 1.0 : 0.0;
		for (; n>=1; n--) ia[n]=0.0;
	} else if (a == 0.0) {
		void bessi(float, int, float []);
		bessi(x,n,ia);
	} else if (a == 0.5) {
		void nonexpspherbessi(float, int, float []);
		float c;
		c=0.797884560802865*sqrt(fabs(x))*exp(fabs(x));
		nonexpspherbessi(x,n,ia);
		for (; n>=0; n--) ia[n] *= c;
	} else {
		void nonexpbessiaplusn(float, float, int, float[]);
		float expx;
		expx=exp(fabs(x));
		nonexpbessiaplusn(a,x,n,ia);
		for (; n>=0; n--) ia[n] *= expx;
	}
}
