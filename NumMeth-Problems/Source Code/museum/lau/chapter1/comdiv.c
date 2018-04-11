#include <math.h>

void comdiv(float xr, float xi, float yr, float yi, float *zr, float *zi)
{
	float h,d;

	if (fabs(yi) < fabs(yr)) {
		if (yi == 0.0) {
			*zr=xr/yr;
			*zi=xi/yr;
		} else {
			h=yi/yr;
			d=h*yi+yr;
			*zr=(xr+h*xi)/d;
			*zi=(xi-h*xr)/d;
		}
	} else {
		h=yr/yi;
		d=h*yr+yi;
		*zr=(xr*h+xi)/d;
		*zi=(xi*h-xr)/d;
	}
}
