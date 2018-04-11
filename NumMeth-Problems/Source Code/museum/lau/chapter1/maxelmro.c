#include <math.h>

int maxelmrow(int l, int u, int i, int j, float **a, float **b, float x)
{
	int k;
	float r, s;

	s=0.0;
	for (k=l; k<=u; k++) {
		r=(a[i][k] += b[j][k]*x);
		if (fabs(r) > s) {
			s=fabs(r);
			l=k;
		}
	}
	return (l);
}
