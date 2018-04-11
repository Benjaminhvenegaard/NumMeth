#include <math.h>

void liniger2(float *x, float xe, int m, float y[], float *sigma1,
			float *sigma2,
			float (*f)(int, float[], int, float *, float *),
			int (*evaluate)(int), float **j,
			void (*jacobian)(int, float **, float [], float *, float *),
			int *k, int itmax, float step, float aeta, float reta,
			void (*output)(float, float, int, float [], float, float,
								float **, int))
{
	int *allocate_integer_vector(int, int);
	float *allocate_real_vector(int, int);
	float **allocate_real_matrix(int, int, int, int);
	void free_integer_vector(int *, int);
	void free_real_vector(float *, int);
	void free_real_matrix(float **, int, int, int);
	float vecvec(int, int, int, float [], float []);
	float matvec(int, int, int, float **, float []);
	float matmat(int, int, int, int, float **, float **);
	void dec(float **, int, float [], int []);
	void sol(float **, int, int [], float []);
	void liniger2coef(int, float **, float **, float [], int [],
			float, float, float, float *, float *, float *,
			float *, float *);
	int i,last,*pi,itnum;
	float h,hl,c0,c1,c2,c3,c4,*dy,*yl,*fl,**a,aux[4],
			jfl,eta,discr;

	pi=allocate_integer_vector(1,m);
	dy=allocate_real_vector(1,m);
	yl=allocate_real_vector(1,m);
	fl=allocate_real_vector(1,m);
	a=allocate_real_matrix(1,m,1,m);

	last=0;
	*k = 0;
	hl=0.0;
	do {
		(*k)++;
		/* step size */
		h=step;
		if (1.1*h >= xe-(*x)) {
			last=1;
			h=xe-(*x);
			(*x)=xe;
		} else
			(*x) += h;
		/* newton iteration */
		itnum=0;
		while (1) {
			itnum++;
			if ((*evaluate)(itnum)) {
				(*jacobian)(m,j,y,sigma1,sigma2);
				liniger2coef(m,j,a,aux,pi,h,*sigma1,*sigma2,
								&c0,&c1,&c2,&c3,&c4);
			} else
				if (itnum == 1 && h != hl)
					liniger2coef(m,j,a,aux,pi,h,*sigma1,*sigma2,
									&c0,&c1,&c2,&c3,&c4);
			for (i=1; i<=m; i++) fl[i]=(*f)(m,y,i,sigma1,sigma2);
			if (itnum == 1)
				for (i=1; i<=m; i++) {
					jfl=matvec(1,m,i,j,fl);
					dy[i]=h*(fl[i]-c4*jfl);
					yl[i]=y[i]+c2*fl[i]+c3*jfl;
				}
			else
				for (i=1; i<=m; i++)
					dy[i]=yl[i]-y[i]+c1*fl[i]-c0*matvec(1,m,i,j,fl);
			sol(a,m,pi,dy);
			for (i=1; i<=m; i++) y[i] += dy[i];
			if (itnum >= itmax) break;
			eta=sqrt(vecvec(1,m,0,y,y))*reta+aeta;
			discr=sqrt(vecvec(1,m,0,dy,dy));
			if (eta >= discr) break;
		}
		hl=h;
		(*output)(*x,xe,m,y,*sigma1,*sigma2,j,*k);
	} while (!last);
	free_integer_vector(pi,1);
	free_real_vector(dy,1);
	free_real_vector(yl,1);
	free_real_vector(fl,1);
	free_real_matrix(a,1,m,1);
}

void liniger2coef(int m, float **j, float **a, float aux[], int pi[],
			float h, float sigma1, float sigma2, float *c0, float *c1,
			float *c2, float *c3, float *c4)
{
	/* this function is internally used by LINIGER2 */

	int i,k,out,doublefit;
	float b1,b2,r,r1,r2,ex,zeta,eta,sinl,cosl,sinh,cosh,d,p,q;

	out=0;
	doublefit=0;
	b1=h*sigma1;
	b2=h*sigma2;
	if (b1 < 0.1) {
		p=0.0;
		q=1.0/3.0;
		out=1;
	}
	if (!out) {
		if (b2 < 0.0) {
			/* complex */
			eta=fabs(b1*sin(sigma2));
			zeta=fabs(b1*cos(sigma2));
			if (eta < b1*b1*1.0e-6) {
				b1=b2=zeta;
				doublefit=1;
			}
			if (!doublefit)
				if (zeta > 40.0) {
					p=1.0-4.0*zeta/b1/b1;
					q=4.0*(1.0-zeta)/b1/b1+1.0;
				} else {
					ex=exp(zeta);
					sinl=sin(eta);
					cosl=cos(eta);
					sinh=0.5*(ex-1.0/ex);
					cosh=0.5*(ex+1.0/ex);
					d=eta*(cosh-cosl)-0.5*b1*b1*sinl;
					p=(zeta*sinl+eta*sinh-
								4.0*zeta*eta/b1/b1*(cosh-cosl))/d;
					q=eta*((cosh-cosl-zeta*sinh-eta*sinl)*
								4.0/b1/b1+cosh+cosl)/d;
				}
		} else if (b1 < 1.0 || b2 < 0.1) {
			/* third order */
			q=1.0/3.0;
			if (b1 > 40.0)
				r=b1/(b1-2.0);
			else {
				ex=exp(-b1);
				r=b1*(1.0-ex)/(b1-2.0+(b1+2.0)*ex);
			}
			p=r/3.0-2.0/b1;
		} else if (fabs(b1-b2) < b1*b1*1.0e-6)
			doublefit=1;
		else {
			if (b1 > 40.0)
				r=b1/(b1-2.0);
			else {
				ex=exp(-b1);
				r=b1*(1.0-ex)/(b1-2.0+(b1+2.0)*ex);
			}
			r1=r*b1;
			if (b2 > 40.0)
				r=b2/(b2-2.0);
			else {
				ex=exp(-b2);
				r=b2*(1.0-ex)/(b2-2.0+(b2+2.0)*ex);
			}
			r2=r*b2;
			d=b2*r1-b1*r2;
			p=2.0*(r2-r1)/d;
			q=2.0*(b2-b1)/d;
		}
		if (doublefit) {
			b1=0.5*(b1+b2);
			if (b1 > 40.0)
				r=b1/(b1-2.0);
			else {
				ex=exp(-b1);
				r=b1*(1.0-ex)/(b1-2.0+(b1+2.0)*ex);
			}
			r1=r;
			if (b1 > 40.0) ex=0.0;
			r2=b1/(1.0-ex);
			r2=1.0-ex*r2*r2;
			q=1.0/(r1*r1*r2);
			p=r1*q-2.0/b1;
		}
	}
	*c0 = 0.25*h*h*(p+q);
	*c1 = 0.5*h*(1.0+p);
	*c2 = h-(*c1);
	*c3 = 0.25*h*h*(q-p);
	*c4 = 0.5*h*p;
	for (i=1; i<=m; i++) {
		for (k=1; k<=m; k++)
			a[i][k]=(*c0)*matmat(1,m,i,k,j,j)-(*c1)*j[i][k];
		a[i][i] += 1.0;
	}
	aux[2]=0.0;
	dec(a,m,aux,pi);
}
