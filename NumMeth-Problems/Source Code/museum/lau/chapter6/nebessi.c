#include <math.h>

void nonexpbessi(float x, int n, float i[])
{
	if (x == 0.0) {
		i[0]=1.0;
		for (; n>=1; n--) i[n]=0.0;
	} else {
		int start(float, int, int);
		int k,negative;
		float x2,r,s;
		negative = (x < 0.0);
		x=fabs(x);
		r=s=0.0;
		x2=2.0/x;
		k=start(x,n,1);
		for (; k>=1; k--) {
			r=1.0/(r+x2*k);
			s=r*(2.0+s);
			if (k <= n) i[k]=r;
		}
		i[0]=r=1.0/(1.0+s);
		if (negative)
			for (k=1; k<=n; k++) r = i[k] *= (-r);
		else
			for (k=1; k<=n; k++) r = i[k] *= r;
	}
}
