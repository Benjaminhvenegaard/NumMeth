#include <math.h>

float infnrmcol(int l, int u, int j, int *k, float **a)
{
	float r, max;

	max=0.0;
	*k=l;
	for (; l<=u; l++) {
		r=fabs(a[l][j]);
		if (r > max) {
			max=r;
			*k=l;
		}
	}
	return (max);
}
