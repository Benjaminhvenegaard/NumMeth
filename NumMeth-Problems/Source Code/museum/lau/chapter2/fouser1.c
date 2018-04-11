#include <math.h>

float fouser1(int n, float theta, float a[], float b[])
{
	int i;
	float r,s,h,co,si;

	r=s=0.0;
	co=cos(theta);
	si=sin(theta);
	for (i=n; i>=1; i--) {
		h=co*r+si*s+a[i];
		s=co*s-si*r+b[i];
		r=h;
	}
	return (co*r+si*s+a[0]);
}
