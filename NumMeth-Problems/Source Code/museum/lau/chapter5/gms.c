#include <float.h>
#include <math.h>

void gms(float *x, float xe, int r, float y[], float h,
			float hmin, float hmax, float *delta,
			void (*derivative)(int, float [], float *),
			void (*jacobian)(int, float **, float [], float *),
			float aeta, float reta, int *n, int *jev, int *lu,
			int nsjev, int linear,
			void (*out)(float, float, int, float [], float,
							int, int, int))
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
	void elmrow(int, int, int, int, float **, float **, float);
	void elmvec(int, int, int, float [], float [], float);
	void dupvec(int, int, int, float [], float []);
	void gsselm(float **, int, float [], int [], int []);
	void solelm(float **, int, int [], int [], float []);
	void colcst(int, int, int, float **, float);
	void mulvec(int, int, int, float [], float [], float);
	void gmsopconstruct(int *, int *, int, float **,
				float **, float **, float [], float [], int [],
				int [], int *, int *, int *, float *, float *,
				float, float, float *, float *,
				void (*)(int, float **, float [], float *));
	void gmscoefficient(float *, float *, float,
				int, int, float *, float *, float,
				float, float **, float **, int);
	void gmsdiffscheme(int, int, int, float [],
				float [], int *, int *, float [], float,
				float **, float **, float, float [], float **,
				float **, float **, int [], int [], float *,
				void (*)(int, float [], float *));
	int k,l,nsjev1,count,count1,kchange,update,change,reeval,
			strategy,*ri,*ci;
	float a,alfa,s1,s2,x0,xl0,xl1,eta,h0,h1,q1,q2,
			discr,aux[10],**bd1,**bd2,*y1,*y0,**hjac,**h2jac2,
			**rqz,*yl,*fl;

	ri=allocate_integer_vector(1,r);
	ci=allocate_integer_vector(1,r);
	y1=allocate_real_vector(1,r);
	y0=allocate_real_vector(1,r);
	yl=allocate_real_vector(1,3*r);
	fl=allocate_real_vector(1,3*r);
	bd1=allocate_real_matrix(1,3,1,3);
	bd2=allocate_real_matrix(1,3,1,3);
	hjac=allocate_real_matrix(1,r,1,r);
	h2jac2=allocate_real_matrix(1,r,1,r);
	rqz=allocate_real_matrix(1,r,1,r);

	/* initialization */
	(*lu)=(*jev)=(*n)=nsjev1=kchange=0;
	x0=(*x);
	discr=0.0;
	k=1;
	h1=h0=h;
	count = -2;
	aux[2]=FLT_EPSILON;
	aux[4]=8.0;
	dupvec(1,r,0,yl,y);
	reeval=change=1;
	strategy=((hmin != hmax) && !linear);
	q1 = -1.0;
	q2 = -2.0;
	count1=0;
	xl0=xl1=0.0;
	(*out)(*x,xe,r,y,*delta,*n,*jev,*lu);
	(*x) += h1;
	/* operator construction */
	gmsopconstruct(&reeval,&update,r,hjac,h2jac2,rqz,y,aux,ri,ci,
			lu,jev,&nsjev1,delta,&alfa,h1,h0,&s1,&s2,jacobian);
	bd1[1][1]=1.0;
	bd2[1][1] = -alfa*0.5;
	if (!linear)
		gmscoefficient(&xl1,&xl0,x0,change,*n,&q1,&q2,h1,
							alfa,bd1,bd2,strategy);
	while (1) {
		gmsdiffscheme(k,count,r,fl,yl,n,&nsjev1,y0,alfa,bd1,
				bd2,h1,y,hjac,h2jac2,rqz,ri,ci,delta,derivative);
		if (strategy) count++;
		if (count == 1) {
			/* test accuracy */
			k=2;
			dupvec(1,r,0,y1,y);
			gmsdiffscheme(k,count,r,fl,yl,n,&nsjev1,y0,alfa,bd1,
					bd2,h1,y,hjac,h2jac2,rqz,ri,ci,delta,derivative);
			k=3;
			eta=aeta+reta*sqrt(vecvec(1,r,0,y1,y1));
			elmvec(1,r,0,y,y1,-1.0);
			discr=sqrt(vecvec(1,r,0,y,y));
			dupvec(1,r,0,y,y1);
		}
		(*out)(*x,xe,r,y,*delta,*n,*jev,*lu);
		if ((*x) >= xe) break;
		/* step size */
		x0=(*x);
		h0=h1;
		if ((*n <= 2) && !linear) (k)++;
		if (count == 1) {
			a=eta/(0.75*(eta+discr))+0.33;
			h1 = (a <= 0.9 || a >= 1.1) ? a*h0 : h0;
			count=0;
			reeval=(a <= 0.9 && nsjev1 != 1);
			count1 = (a >= 1.0 || reeval) ? 0 : count1+1;
			if (count1 == 10) {
				count1=0;
				reeval=1;
				h1=a*h0;
			}
		} else {
			h1=h;
			reeval=((nsjev == nsjev1) && !strategy && !linear);
		}
		if (strategy)
			h1 = (h1 > hmax) ? hmax : ((h1 < hmin) ? hmin : h1);
		(*x) += h1;
		if ((*x) >= xe) {
			h1=xe-x0;
			(*x)=xe;
		}
		if ((*n <= 2) && !linear) reeval=1;
		if (h1 != h0) {
			update=1;
			kchange=3;
		}
		if (reeval) update=1;
		change=((kchange > 0) && !linear);
		kchange--;
		if (update)
			/* operator construction */
			gmsopconstruct(&reeval,&update,r,hjac,h2jac2,rqz,y,aux,
				ri,ci,lu,jev,&nsjev1,delta,&alfa,h1,h0,&s1,&s2,
				jacobian);
		if (!linear)
			gmscoefficient(&xl1,&xl0,x0,change,*n,&q1,&q2,h1,
								alfa,bd1,bd2,strategy);
		/* next integration step */
		for (l=2; l>=1; l--) {
			dupvec(l*r+1,(l+1)*r,-r,yl,yl);
			dupvec(l*r+1,(l+1)*r,-r,fl,fl);
		}
		dupvec(1,r,0,yl,y);
	}
	free_integer_vector(ri,1);
	free_integer_vector(ci,1);
	free_real_vector(y1,1);
	free_real_vector(y0,1);
	free_real_vector(yl,1);
	free_real_vector(fl,1);
	free_real_matrix(bd1,1,3,1);
	free_real_matrix(bd2,1,3,1);
	free_real_matrix(hjac,1,r,1);
	free_real_matrix(h2jac2,1,r,1);
	free_real_matrix(rqz,1,r,1);
}

void gmsopconstruct(int *reeval, int *update, int r,
		float **hjac, float **h2jac2, float **rqz, float y[],
		float aux[], int ri[], int ci[], int *lu, int *jev,
		int *nsjev1, float *delta, float *alfa, float h1,
		float h0, float *s1, float *s2,
		void (*jacobian)(int, float **, float [], float *))
{
	/* this function is internally used by GMS */

	int i,j;
	float a,a1,z1,e,q;

	if (*reeval) {
		(*jacobian)(r,hjac,y,delta);
		(*jev)++;
		*nsjev1 = 0;
		if (*delta <= 1.0e-15)
			(*alfa)=1.0/3.0;
		else {
			z1=h1*(*delta);
			a=z1*z1+12.0;
			a1=6.0*z1;
			if (fabs(z1) < 0.1)
				(*alfa)=(z1*z1/140.0-1.0)*z1/30.0;
			else if (z1 < -33.0)
				(*alfa)=(a+a1)/(3.0*z1*(2.0+z1));
			else {
				e=exp(z1);
				(*alfa)=((a-a1)*e-a-a1)/(((2.0-z1)*e-2.0-z1)*z1*3.0);
			}
		}
		(*s1) = -(1.0+(*alfa))*0.5;
		(*s2)=((*alfa)*3.0+1.0)/12.0;
	}
	a=h1/h0;
	a1=a*a;
	if (*reeval) a=h1;
	if (a != 1.0)
		for (j=1; j<=r; j++) colcst(1,r,j,hjac,a);
	for (i=1; i<=r; i++) {
		for (j=1; j<=r; j++) {
			q=h2jac2[i][j]=(*reeval ? matmat(1,r,i,j,hjac,hjac) :
									h2jac2[i][j]*a1);
			rqz[i][j]=(*s2)*q;
		}
		rqz[i][i] += 1.0;
		elmrow(1,r,i,i,rqz,hjac,*s1);
	}
	gsselm(rqz,r,aux,ri,ci);
	(*lu)++;
	(*reeval)=(*update)=0;
}

void gmscoefficient(float *xl1, float *xl0, float x0,
		int change, int n, float *q1, float *q2, float h1,
		float alfa, float **bd1, float **bd2, int strategy)
{
	/* this function is internally used by GMS */

	float a,q12,q22,q1q2,xl2;

	xl2=(*xl1);
	(*xl1)=(*xl0);
	(*xl0)=x0;
	if (change) {
		if (n > 2) {
			(*q1)=((*xl1)-(*xl0))/h1;
			(*q2)=(xl2-(*xl0))/h1;
		}
		q12=(*q1)*(*q1);
		q22=(*q2)*(*q2);
		q1q2=(*q1)*(*q2);
		a = -(3.0*alfa+1.0)/12.0;
		bd1[1][3]=1.0+(1.0/3.0-((*q1)+(*q2))*0.5)/q1q2;
		bd1[2][3]=(1.0/3.0-(*q2)*0.5)/(q12-q1q2);
		bd1[3][3]=(1.0/3.0-(*q1)*0.5)/(q22-q1q2);
		bd2[1][3] = -alfa*0.5+a*(1.0-(*q1)-(*q2))/q1q2;
		bd2[2][3]=a*(1.0-(*q2))/(q12-q1q2);
		bd2[3][3]=a*(1.0-(*q1))/(q22-q1q2);
		if (strategy || n <= 2) {
			bd1[2][2]=1.0/(2.0*(*q1));
			bd1[1][2]=1.0-bd1[2][2];
			bd2[2][2] = -(3.0*alfa+1.0)/(12.0*(*q1));
			bd2[1][2] = -bd2[2][2]-alfa*0.5;
		}
	}
}

void gmsdiffscheme(int k, int count, int r, float fl[],
			float yl[], int *n, int *nsjev1, float y0[],
			float alfa, float **bd1, float **bd2, float h1,
			float y[], float **hjac, float **h2jac2, float **rqz,
			int ri[], int ci[], float *delta,
			void (*derivative)(int, float [], float *))
{
	/* this function is internally used by GMS */

	int i,l;

	if (count != 1) {
		dupvec(1,r,0,fl,yl);
		(*derivative)(r,fl,delta);
		(*n)++;
		(*nsjev1)++;
	}
	mulvec(1,r,0,y0,yl,(1.0-alfa)/2.0-bd1[1][k]);
	for (l=2; l<=k; l++) elmvec(1,r,r*(l-1),y0,yl,-bd1[l][k]);
	for (l=1; l<=k; l++) elmvec(1,r,r*(l-1),y0,fl,h1*bd2[l][k]);
	for (i=1; i<=r; i++) y[i]=matvec(1,r,i,hjac,y0);
	mulvec(1,r,0,y0,yl,(1.0-3.0*alfa)/12.0-bd2[1][k]);
	for (l=2; l<=k; l++) elmvec(1,r,r*(l-1),y0,yl,-bd2[l][k]);
	for (i=1; i<=r; i++) y[i] += matvec(1,r,i,h2jac2,y0);
	dupvec(1,r,0,y0,yl);
	for (l=1; l<=k; l++) elmvec(1,r,r*(l-1),y0,fl,h1*bd1[l][k]);
	elmvec(1,r,0,y,y0,1.0);
	solelm(rqz,r,ri,ci,y);
}

