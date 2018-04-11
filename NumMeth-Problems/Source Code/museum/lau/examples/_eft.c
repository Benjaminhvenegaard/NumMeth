#include <math.h>
#include <stdio.h>

float expt,lnt,u0,u1,u2,accuracy;

void derivative(float t, int m0, int m, int i, float u[])
{
	if (i == 1) {
		expt=exp(t);
		lnt=log(t);
		u0=u[0];
		u1=u[0]=expt*(lnt-u0)+1.0/t;
	} else if (i == 2)
		u2=u[0]=expt*(lnt-u0-u1+1.0/t)-1.0/t/t;
	else
		u[0]=expt*(lnt-u0-2.0*u1-u2+2.0/t-1.0/t/t)+2.0/t/t/t;
}

float sigma(float t, int m0, int m)
{
	return exp(t);
}

float diameter(float t, int m0, int m)
{
	return 2.0*exp(2.0*t/3.0);
}

float aeta1(float t, int m0, int m)
{
	return accuracy/10.0;
}

float aeta2(float t, int m0, int m)
{
	return accuracy/10.0*((t < 3.0) ? 1.0 : exp(2.0*(t-3.0)));
}

float reta1(float t, int m0, int m)
{
	return accuracy;
}

float reta2(float t, int m0, int m)
{
	return accuracy*((t < 3.0) ? 1.0 : exp(2.0*(t-3.0)));
}

void out(float t, float te, int m0, int m, float u[],
			int k, float eta, float rho)
{
	if (t == te) printf(" %3d  %e  ",k,u[0]);
}

void main ()
{
	void eft(float *, float, int, int, float [],
			float (*)(float, int, int), float,
			float (*)(float, int, int),
			void (*)(float, int, int, int, float []),
			int *, float, int,
			float (*)(float, int, int), float (*)(float, int, int),
			float *, float *, float, float *,
			void (*)(float, float, int, int, float [],
							int, float, float));
	int j,k,l;
	float t,te,te1,te2,eta,rho,pi,hs,u[1];

	printf("The results with EFT are:\n"
		"   K     U(TE1)       K     U(TE2)      RETA\n");
	pi=4.0*atan(1.0);
	te1=exp(1.0);
	te2=exp(2.0);
	accuracy=1.0;
	for (j=1; j<=4; j++) {
		accuracy *= 1.0e-1;
		t=0.01;
		u[0]=log(t);
		k=0;
		hs=0.0;
		for (l=1; l<=2; l++) {
			te = (l == 1) ? te1 : te2;
			eft(&t,te,0,0,u,sigma,pi,diameter,derivative,&k,
				1.5,2,aeta1,reta1,&eta,&rho,1.0e-4,&hs,out);
		}
		printf(" %6.1e\n",accuracy);
	}
	printf("\nWith relaxed accuracy conditions for t > 3 :\n"
		"   K     U(TE1)       K     U(TE2)      RETA\n");
	accuracy=1.0;
	for (j=1; j<=4; j++) {
		accuracy *= 1.0e-1;
		t=0.01;
		u[0]=log(t);
		k=0;
		hs=0.0;
		for (l=1; l<=2; l++) {
			te = (l == 1) ? te1 : te2;
			eft(&t,te,0,0,u,sigma,pi,diameter,derivative,&k,
				1.5,2,aeta2,reta2,&eta,&rho,1.0e-4,&hs,out);
		}
		printf(" %6.1e\n",accuracy);
	}
}

