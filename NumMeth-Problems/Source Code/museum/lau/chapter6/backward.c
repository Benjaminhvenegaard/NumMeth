#include <math.h>

void backward(float x, float p, float q, float i0, int nmax,
				float eps, float i[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	int m,n,nu,finish;
	float r,pq,y,logx,*iapprox;

	iapprox=allocate_real_vector(0,nmax);
	i[0]=i0;
	if (nmax > 0) {
		for (n=1; n<=nmax; n++) iapprox[n]=0.0;
		pq=p+q-1.0;
		logx=log(x);
		r=nmax+(log(eps)+q*log(nmax))/logx;
		nu=floor(r-q*log(r)/logx);
		while (1) {
			n=nu;
			r=x;
			while (1) {
				y=(n+pq)*x;
				r=y/(y+(n+p)*(1.0-r));
				if (n <= nmax) i[n]=r;
				n--;
				if (n < 1) break;
			}
			r=i0;
			for (n=1; n<=nmax; n++) r = i[n] *= r;
			finish=1;
			for (n=1; n<=nmax; n++)
				if (fabs((i[n]-iapprox[n])/i[n]) > eps) {
					for (m=1; m<=nmax; m++) iapprox[m]=i[m];
					nu += 5;
					finish=0;
					break;
				}
			if (finish) break;
		}
	}
	free_real_vector(iapprox,0);
}
