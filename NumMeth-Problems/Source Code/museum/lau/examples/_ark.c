#include <math.h>
#include <stdio.h>

void der1(int *m0, int *m, float *t, float v[])
{
	v[1] -= 2.0*(*t)/v[1];
}

void der2(int *m0, int *m, float *t, float v[])
{
	int j;
	float v1,v2,v3;

	v2=v[*m0];
	(*m0)++;
	(*m)--;
	v3=v[*m0];
	for (j=(*m0); j<=(*m); j++) {
		v1=v2;
		v2=v3;
		v3=v[j+1];
		v[j]=250.0*(v3-v1)/3.0;
	}
}

void out1(int *m0, int *m, float *t, float *te, float y[],
			float data[])
{
	if (*t == *te) {
		if (*t == 1.0)
			printf("\nProblem 1\n\n"
				" x   integration steps  y(computed)    y(exact)\n");
		printf("%2.0f        %3.0f           %e   %e\n",
				*t,data[8],y[1],sqrt(2.0*(*t)+1));
		*te = 2.0;
	}
}

void out2(int *m0, int *m, float *t, float *te, float u[],
			float data[])
{
	if (fabs((*t)-0.6) < 1.0e-5)
		printf("\n\nProblem 2\n\n"
			" derivative calls   u(.6,0)(computed)   u(.6,0)exact\n"
			"    %4.0f               %e       %e\n",
			data[1]*data[8],u[0],exp(-0.09));
}

void main ()
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void ark(float *, float *, int *, int *, float [],
			void (*)(int *, int *, float *, float[]), float [],
			void (*)(int *, int *, float *, float *,
						float [], float []));
	int m0,m,i;
	static float dat1[13]={3.0, 3.0, 1.0, 1.0, 1.0e-3, 1.0e-6,
				1.0e-6, 0.0, 0.0, 0.0, 1.0, 0.5, 1.0/6.0};
	static float dat2[14]={4.0, 3.0, 0.0, 500.0/3.0, 0.0,
			-1.0, -1.0, 0.0, 0.0, 0.0, 1.0, 0.5, 1.0/6.0, 1.0/24.0};
	float t,te,y[2],*u,data[15];

	u=allocate_real_vector(-150,150);
	for (i=1; i<=13; i++) data[i]=dat1[i-1];
	t=0.0;
	y[1]=1.0;
	te=1.0;
	m0=m=1;
	ark(&t,&te,&m0,&m,y,der1,data,out1);
	for (i=1; i<=14; i++) data[i]=dat2[i-1];
	data[3]=sqrt(8.0);
	data[5]=data[3]/data[4];
	m0 = -150;
	m=150;
	t=0.0;
	u[0]=1.0;
	for (i=1; i<=m; i++) u[i]=u[-i]=exp(-(0.003*i)*(0.003*i));
	te=0.6;
	ark(&t,&te,&m0,&m,u,der2,data,out2);
	free_real_vector(u,-150);
}

