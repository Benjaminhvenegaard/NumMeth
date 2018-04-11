#include <math.h>

float infnrmrow(int l, int u, int i, int *k, float **a)
{
	float r, max;

	max=0.0;
	*k=l;
	for (; l<=u; l++) {
		r=fabs(a[i][l]);
		if (r > max) {
			max=r;
			*k=l;
		}
	}
	return (max);
}
