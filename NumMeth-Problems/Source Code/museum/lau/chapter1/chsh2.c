#include <math.h>

void chsh2(float a1r, float a1i, float a2r, float a2i,
				float *c, float *sr, float *si)
{
	float r;

	if (a2r != 0.0 || a2i != 0.0) {
		if (a1r != 0.0 || a1i != 0.0) {
			r=sqrt(a1r*a1r+a1i*a1i);
			*c=r;
			*sr=(a1r*a2r+a1i*a2i)/r;
			*si=(a1r*a2i-a1i*a2r)/r;
			r=sqrt(*c * *c + *sr * *sr + *si * *si);
			*c /= r;
			*sr /= r;
			*si /= r;
		} else {
			*si = *c = 0.0;
			*sr=1.0;
		}
	} else {
		*c=1.0;
		*sr = *si = 0.0;
	}
}
