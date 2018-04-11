#include <math.h>

void linemin(int n, float x[], float d[], float nd, float *alfa,
				float g[], float (*funct)(int, float[], float[]),
				float f0, float *f1, float df0, float *df1, int *evlmax,
				int strongsearch, float in[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float vecvec(int, int, int, float [], float []);
	void elmvec(int, int, int, float [], float [], float);
	void dupvec(int, int, int, float [], float []);
	int evl,notinint;
	float f,oldf,df,olddf,mu,alfa0,q,w,y,z,reltol,abstol,eps,aid,*x0;

	x0=allocate_real_vector(1,n);
	reltol=in[1];
	abstol=in[2];
	mu=in[3];
	evl=0;
	alfa0=0.0;
	oldf=f0;
	olddf=df0;
	y = *alfa;
	notinint=1;
	dupvec(1,n,0,x0,x);
	eps=(sqrt(vecvec(1,n,0,x,x))*reltol+abstol)/nd;
	q=((*f1)-f0)/((*alfa)*df0);
	while (1) {
		if (notinint) notinint = ((*df1) < 0.0 && q > mu);
		aid = *alfa;
		if (*df1 >= 0.0) {
			/* cubic interpolation */
			z=3.0*(oldf-(*f1))/(*alfa)+olddf+(*df1);
			w=sqrt(z*z-olddf*(*df1));
			*alfa = (*alfa)*(1.0-((*df1)+w-z)/((*df1)-olddf+w*2.0));
			if (*alfa < eps)
				*alfa=eps;
			else
				if (aid-(*alfa) < eps) *alfa=aid-eps;
		} else
			if (notinint) {
				alfa0 = *alfa = y;
				olddf = *df1;
				oldf = *f1;
			} else
				*alfa *= 0.5;
		y = (*alfa)+alfa0;
		dupvec(1,n,0,x,x0);
		elmvec(1,n,0,x,d,y);
		eps=(sqrt(vecvec(1,n,0,x,x))*reltol+abstol)/nd;
		f=(*funct)(n,x,g);
		evl++;
		df=vecvec(1,n,0,d,g);
		q=(f-f0)/(y*df0);
		if (!(((notinint || strongsearch) ? 1 : (q < mu || q > 1.0-mu))
				&& (evl < *evlmax))) break;
		if (notinint || df > 0.0 || q < mu) {
			*df1=df;
			*f1=f;
		} else {
			alfa0=y;
			*alfa=aid-(*alfa);
			olddf=df;
			oldf=f;
		}
		if (*alfa <= eps*2.0) break;
	}
	*alfa=y;
	*evlmax=evl;
	*df1=df;
	*f1=f;
	free_real_vector(x0,1);
}
