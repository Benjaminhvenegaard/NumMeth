#include <math.h>

float onenrmvec(int l, int u, float a[])
{
	float sum;

	sum=0.0;
	for (; l<=u; l++) sum += fabs(a[l]);
	return (sum);
}
