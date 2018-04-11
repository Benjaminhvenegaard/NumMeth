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

float df(float x)
{
	int i;
	float s,temp1,temp2;

	s=0.0;
	for (i=1; i<=20; i++) {
		temp1=i*2-5;
		temp2=x-i*i;
		s += (temp1*temp1)/(temp2*temp2*temp2);
	}
	return -s*2.0;
}

float tol(float x)
{
	return (fabs(x)*1.0e-6+1.0e-6);
}

void main ()
{
	float mininder(float *, float *, float (*)(float),
						float (*)(float), float (*)(float));
	float m,x,y;

	x=1.01;
	y=3.99;
	m=mininder(&x,&y,f,df,tol);
	printf("Minimum is  %e\nFor x is  %e  and y is  %e\n",m,x,y);
}

