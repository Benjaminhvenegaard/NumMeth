#include <math.h>

float determbnd(float a[], int n, int lw, int rw, int sgndet)
{
	int i,l;
	float p;

	l=1;
	p=1.0;
	lw += rw+1;
	for (i=1; i<=n; i++) {
		p=a[l]*p;
		l += lw;
	}
	return (fabs(p)*sgndet);
}
