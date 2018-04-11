#include <math.h>

void bessyaplusn(float a, float x, int nmax, float yan[])
{
	void bessya01(float, float, float *, float *);
	int n;
	float y1;

	bessya01(a,x,&yan[0],&y1);
	a -= 1.0;
	x=2.0/x;
	if (nmax > 0) yan[1]=y1;
	for (n=2; n<=nmax; n++) yan[n] = -yan[n-2]+(a+n)*x*yan[n-1];
}
