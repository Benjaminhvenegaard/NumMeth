#include <math.h>

float comabs(float xr, float xi)
{
	float temp;

	xr=fabs(xr);
	xi=fabs(xi);
	if (xi > xr) {
		temp=xr/xi;
		return (sqrt(temp*temp+1.0)*xi);
	}
	if (xi == 0.0)
		return (xr);
	else {
		temp=xi/xr;
		return (sqrt(temp*temp+1.0)*xr);
	}
}
