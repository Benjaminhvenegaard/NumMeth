#include <math.h>

float onenrmcol(int l, int u, int j, float **a)
{
	float sum;

	sum=0.0;
	for (; l<=u; l++) sum += fabs(a[l][j]);
	return (sum);
}
