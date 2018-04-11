#include <math.h>

float onenrmrow(int l, int u, int i, float **a)
{
	float sum;

	sum=0.0;
	for (; l<=u; l++) sum += fabs(a[i][l]);
	return (sum);
}
