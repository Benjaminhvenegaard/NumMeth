#include <float.h>
#include <math.h>

void valsymtri(float d[], float bb[], int n, int n1, int n2,
					float val[], float em[])
{
	float sturm(float [], float [], int, float, int, float,
					float, int *, float *, float *);
	int k,count,ext,extrapolate;
	float max,x,y,macheps,norm,re,machtol,ub,lb,lambda,
			c,fc,b,fb,a,fa,dd,fd,fdb,fda,w,mb,tol,m,p,q;

	macheps=em[0];
	norm=em[1];
	re=em[2];
	machtol=norm*macheps;
	max=norm/macheps;
	count=0;
	ub=1.1*norm;
	lb = -ub;
	lambda=ub;
	for (k=n1; k<=n2; k++) {
		y=ub;
		lb = -1.1*norm;
		x=lb;

		/* look for the zero of the polynomial function */

		b=x;
		fb=sturm(d,bb,n,x,k,machtol,max,&count,&lb,&ub);
		a=x=y;
		fa=sturm(d,bb,n,x,k,machtol,max,&count,&lb,&ub);
		c=a;
		fc=fa;
		ext=0;
		extrapolate=1;
		while (extrapolate) {
			if (fabs(fc) < fabs(fb)) {
				if (c != a) {
					dd=a;
					fd=fa;
				}
				a=b;
				fa=fb;
				b=x=c;
				fb=fc;
				c=a;
				fc=fa;
			}
			tol=fabs(x)*re+machtol;
			m=(c+b)*0.5;
			mb=m-b;
			if (fabs(mb) > tol) {
				if (ext > 2)
					w=mb;
				else {
					if (mb == 0.0)
						tol=0.0;
					else
						if (mb < 0.0) tol = -tol;
					p=(b-a)*fb;
					if (ext <= 1)
						q=fa-fb;
					else {
						fdb=(fd-fb)/(dd-b);
						fda=(fd-fa)/(dd-a);
						p *= fda;
						q=fdb*fa-fda*fb;
					}
					if (p < 0.0) {
						p = -p;
						q = -q;
					}
					w=(p<FLT_MIN || p<=q*tol) ? tol : ((p<mb*q) ? p/q : mb);
				}
				dd=a;
				fd=fa;
				a=b;
				fa=fb;
				x = b += w;
				fb=sturm(d,bb,n,x,k,machtol,max,&count,&lb,&ub);
				if ((fc >= 0.0) ? (fb >= 0.0) : (fb <= 0.0)) {
					c=a;
					fc=fa;
					ext=0;
				} else
					ext = (w == mb) ? 0 : ext+1;
			} else
				break;
		}
		y=c;

		/* end of the zero finding procedure */

		val[k] = lambda = (x > lambda) ? lambda : x;
		if (ub > x)
			ub = (x > y) ? x : y;
	}
	em[3]=count;
}

float sturm(float d[], float bb[], int n, float x, int k, float machtol,
				float max, int *count, float *lb, float *ub)
{
	/* this sturm sequence procedure is used internally by VALSYMTRI */

	int p,i;
	float f;

	(*count)++;
	p=k;
	f=d[1]-x;
	for (i=2; i<=n; i++) {
		if (f <= 0.0) {
			p++;
			if (p > n) return ((p == n) ? f : (n-p)*max);
		} else
			if (p < i-1) {
				*lb = x;
				return ((p == n) ? f : (n-p)*max);
			}
		if (fabs(f) < machtol)
			f = (f <= 0.0) ? -machtol : machtol;
		f=d[i]-x-bb[i-1]/f;
	}
	if (p == n || f <= 0.0)
		if (x < *ub) *ub = x;
	else
		*lb = x;
	return ((p == n) ? f : (n-p)*max);
}
