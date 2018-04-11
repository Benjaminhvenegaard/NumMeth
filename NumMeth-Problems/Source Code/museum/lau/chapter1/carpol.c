#include <math.h>

void carpol(float ar, float ai, float *r, float *c, float *s)
{
	float temp;

	if (ar == 0.0 && ai == 0.0) {
		*c = 1.0;
		*r = *s = 0.0;
	} else {
		if (fabs(ar) > fabs(ai)) {
			temp=ai/ar;
			*r = fabs(ar)*sqrt(1.0+temp*temp);
		} else {
			temp=ar/ai;
			*r = fabs(ai)*sqrt(1.0+temp*temp);
		}
		*c = ar / *r;
		*s = ai / *r;
	}
}
