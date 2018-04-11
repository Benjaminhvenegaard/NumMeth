#include <math.h>

float comeucnrm(float **ar, float **ai, int lw, int n)
{
	float mattam(int, int, int, int, float **, float **);
	int i,l;
	float r;

	r=0.0;
	for (i=1; i<=n; i++) {
		l=(i>lw) ? i-lw : 1;
		r += mattam(l,n,i,i,ar,ar)+mattam(l,n,i,i,ai,ai);
	}
	return (sqrt(r));
}
