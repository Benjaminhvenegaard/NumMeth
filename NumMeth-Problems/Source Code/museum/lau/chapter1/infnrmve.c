#include <math.h>

float infnrmvec(int l, int u, int *k, float a[])
{
	float r, max;

	max=0.0;
	*k=l;
	for (; l<=u; l++) {
		r=fabs(a[l]);
		if (r > max) {
			max=r;
			*k=l;
		}
	}
	return (max);
}
