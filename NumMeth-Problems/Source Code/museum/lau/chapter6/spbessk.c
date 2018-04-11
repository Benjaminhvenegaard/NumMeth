#include <math.h>

void spherbessk(float x, int n, float k[])
{
	void nonexpspherbessk(float, int, float []);
	float expx;
	expx=exp(-x);
	nonexpspherbessk(x,n,k);
	for (; n>=0; n--) k[n] *= expx;
}
