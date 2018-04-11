#include <math.h>

void spherbessi(float x, int n, float i[])
{
	if (x == 0.0) {
		i[0]=1.0;
		for (; n>=1; n--) i[n]=0.0;
	} else {
		void nonexpspherbessi(float, int, float []);
		float expx;
		expx=exp(x);
		nonexpspherbessi(x,n,i);
		for (; n>=0; n--) i[n] *= expx;
	}
}
