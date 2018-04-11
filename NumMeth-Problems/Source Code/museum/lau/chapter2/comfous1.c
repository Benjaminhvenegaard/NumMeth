#include <math.h>

void comfouser1(int n, float theta, float ar[], float ai[],
					float *rr, float *ri)
{
	int k;
	float h,hr,hi,co,si;

	hr=hi=0.0;
	co=cos(theta);
	si=sin(theta);
	for (k=n; k>=1; k--) {
		h=co*hr-si*hi+ar[k];
		hi=co*hi+si*hr+ai[k];
		hr=h;
	}
	*rr=co*hr-si*hi+ar[0];
	*ri=co*hi+si*hr+ai[0];
}
