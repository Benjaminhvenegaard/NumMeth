#include <math.h>

float determ(float **a, int n, int sign)
{
	int i;
	float det;

	det=1.0;
	for (i=1; i<=n; i++) det *= a[i][i];
	return (sign*fabs(det));
}
