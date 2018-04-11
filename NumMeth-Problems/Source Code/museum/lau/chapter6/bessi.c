#include <math.h>

void bessi(float x, int n, float i[])
{
	if (x == 0.0) {
		i[0]=1.0;
		for (; n>=1; n--) i[n]=0.0;
	} else {
		void nonexpbessi(float, int, float []);
		float expx;
		expx=exp(fabs(x));
		nonexpbessi(x,n,i);
		for (; n>=0; n--) i[n] *= expx;
	}
}
