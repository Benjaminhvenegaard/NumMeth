#include <stdio.h>
#include <math.h>

float f(float x)
{
	int i;
	float s,temp;

	s=0.0;
	for (i=1; i<=20; i++) {
		temp=(i*2-5)/(x-i*i);
		s += temp*temp;
	}
	return s;
}

float tol(float x)
{
	return (fabs(x)*1.0e-6+1.0e-6);
}

void main ()
{
	float minin(float *, float *, float *, float (*)(float),
					float (*)(float));
	float m,x,a,b;

	a=1.0+tol(1.0);
	b=4.0-tol(4.0);
	m=minin(&x,&a,&b,f,tol);
	printf("Minimum is  %e\nFor x is  %e\nin the interval with "
			"endpoints  %e   %e",m,x,a,b);
}

