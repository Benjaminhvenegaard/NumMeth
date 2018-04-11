#include <math.h>

void arkmat(float *t, float te, int m, int n, float **u,
			void (*der)(int, int, float, float **, float **),
			int type, int *order, float *spr,
			void (*out)(float, float, int, int, float **, int,
							int, float *))
{
	float **allocate_real_matrix(int, int, int, int);
	void free_real_matrix(float **, int, int, int);
	void elmcol(int, int, int, int, float **, float **, float);
	void dupmat(int, int, int, int, float **, float **);
	int sig,l,last,ta,tb,i;
	float tau,lambda[10],**uh,**du,mlt;
	static float lbd1[9]={1.0/9.0, 1.0/8.0, 1.0/7.0, 1.0/6.0,
						1.0/5.0, 1.0/4.0, 1.0/3.0, 1.0/2.0, 4.3};
	static float lbd2[9]={0.1418519249e-2, 0.3404154076e-2,
					0.0063118569, 0.01082794375, 0.01842733851,
					0.03278507942, 0.0653627415, 0.1691078577, 156.0};
	static float lbd3[9]={0.3534355908e-2, 0.8532600867e-2,
					0.015956206, 0.02772229155, 0.04812587964,
					0.08848689452, 0.1863578961, 0.5, 64.0};
	static float lbd4[9]={1.0/8.0, 1.0/20.0, 5.0/32.0, 2.0/17.0,
					17.0/80.0, 5.0/22.0, 11.0/32.0, 1.0/2.0, 8.0};

	uh=allocate_real_matrix(1,n,1,m);
	du=allocate_real_matrix(1,n,1,m);

	/* initialize */
	if (type != 2 && type != 3) type=1;
	if (type != 2)
		*order = 2;
	else
		if (*order != 2) *order = 1;
	switch ((type == 1) ? 1 : type+(*order)-1) {
		case 1:  for (i=0; i<=8; i++) lambda[i+1]=lbd1[i]; break;
		case 2:  for (i=0; i<=8; i++) lambda[i+1]=lbd2[i]; break;
		case 3:  for (i=0; i<=8; i++) lambda[i+1]=lbd3[i]; break;
		case 4:  for (i=0; i<=8; i++) lambda[i+1]=lbd4[i]; break;
	}
	sig = ((te == *t) ? 0 : ((te > *t) ? 1 : -1));
	last=0;
	do {
		tau=((*spr == 0.0) ? fabs(te-(*t)) :
					fabs(lambda[9]/(*spr)))*sig;
		ta = (*t)+tau >= te;
		tb = tau >= 0.0;
		if ((ta && tb) || (!(ta || tb))) {
			tau=te-(*t);
			last=1;
		}
		/* difference scheme */
		(*der)(m,n,*t,u,du);
		for (i=1; i<=8; i++) {
			mlt=lambda[i]*tau;
			dupmat(1,n,1,m,uh,u);
			for (l=1; l<=m; l++) elmcol(1,n,l,l,uh,du,mlt);
			(*der)(m,n,(*t)+mlt,uh,du);
		}
		for (l=1; l<=m; l++) elmcol(1,n,l,l,u,du,tau);
		*t = (last ? te : (*t)+tau);
		(*out)(*t,te,m,n,u,type,*order,spr);
	} while (!last);
	free_real_matrix(uh,1,n,1);
	free_real_matrix(du,1,n,1);
}

