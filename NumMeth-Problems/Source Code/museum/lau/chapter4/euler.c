#include <math.h>

float euler(float (*ai)(int), float eps, int tim)
{
	int i,k,n,t;
	float mn,mp,ds,sum,m[16];

	n=t=i=0;
	m[0]=(*ai)(i);
	sum=m[0]/2.0;
	do {
		i++;
		mn=(*ai)(i);
		for (k=0; k<=n; k++) {
			mp=(mn+m[k])/2.0;
			m[k]=mn;
			mn=mp;
		}
		if (fabs(mn) < fabs(m[n]) && n < 15) {
			ds=mn/2.0;
			n++;
			m[n]=mn;
		} else
			ds=mn;
		sum += ds;
		t = (fabs(ds) < eps) ? t+1 : 0;
	} while (t < tim);
	return sum;
}
